package io.github.luminion.sqlbooster.model.sql.helper.processor;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.api.Sort;
import io.github.luminion.sqlbooster.model.api.Tree;
import io.github.luminion.sqlbooster.model.sql.SqlCondition;
import io.github.luminion.sqlbooster.model.sql.SqlSort;
import io.github.luminion.sqlbooster.model.sql.SqlTree;
import io.github.luminion.sqlbooster.model.sql.helper.AbstractHelper;
import io.github.luminion.sqlbooster.model.sql.helper.BaseHelper;
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;
import io.github.luminion.sqlbooster.util.BoostUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 默认 SQL 处理器.
 * <p>
 * 提供 SQL 条件的验证和处理功能, 包括字段映射验证、操作符验证等.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class BasicProcessor {

    /**
     * 验证并处理单个 SQL 条件.
     * <p>
     * <ul>
     *     <li>验证字段是否存在于映射中.</li>
     *     <li>验证操作符和值的合法性.</li>
     *     <li>处理 LIKE 操作符的值.</li>
     *     <li>将不符合条件的字段或无法处理的条件放入 `extra` Map 中.</li>
     * </ul>
     *
     * @param condition                待验证的 SQL 条件
     * @param propertyToColumnAliasMap 属性名到数据库列别名的映射
     * @param extra                    用于存储额外参数的 Map
     * @return 验证并处理后的 {@link Condition}, 如果条件无效则返回 null
     * @since 1.0.0
     */
    public static Condition validateCondition(Condition condition, Map<String, String> propertyToColumnAliasMap, Map<String, Object> extra) {
        String field = condition.getField();
        String operator = condition.getOperator();
        Object value = condition.getValue();
        if (field == null || field.isEmpty()) {
            return null;
        }
        String jdbcColumn = propertyToColumnAliasMap.get(field);
        if (jdbcColumn == null) {
            log.debug("condition field [{}] not exist in fieldMap , it will be removed and put into paramMap", field);
            extra.putIfAbsent(field, value);
            return null;
        }
        operator = SqlKeyword.replaceOperator(operator);
        if (!SqlKeyword.isNullOperator(operator) && value == null) {
            log.info("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
            extra.putIfAbsent(field, "null");
            return null;
        }
        if (SqlKeyword.isInOperator(operator)) {
            boolean iterableValue = value instanceof Iterable;
            if (!iterableValue) {
                log.debug("condition field [{}] requires collection but value is not iterable, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
            Iterable<?> iterable = (Iterable<?>) value;
            if (!iterable.iterator().hasNext()) {
                log.debug("condition field [{}] requires collection but value is empty, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
        }
        if (SqlKeyword.isBitOperator(operator)) {
            boolean isNumber = value instanceof Number;
            if (!isNumber) {
                log.debug("condition field [{}] requires number but value is not a number, it will be removed and put into paramMap", field);
                extra.putIfAbsent(field, value);
                return null;
            }
        }
        if (SqlKeyword.isLikeOperator(operator)) {
//            if(!(value instanceof String)){
//                log.debug("condition field [{}] requires string but value is not string, it will be removed and put into paramMap", field);
//                extra.putIfAbsent(field, value);
//                return null;
//            }
            if (!value.toString().contains("%")) {
                value = "%" + value + "%";
            }
        }
        return new SqlCondition(jdbcColumn, operator, value);
    }

    /**
     * 验证并处理单个 SQL 排序规则.
     *
     * @param sort                     待验证的 SQL 排序规则
     * @param propertyToColumnAliasMap 属性名到数据库列名的映射
     * @return 验证并处理后的 {@link Sort}, 如果排序字段无效则返回 null
     * @since 1.0.0
     */
    public static Sort validateSort(Sort sort, Map<String, String> propertyToColumnAliasMap) {
        String jdbcColumn = propertyToColumnAliasMap.get(sort.getField());
        if (jdbcColumn == null) {
            log.warn("sort field [{}] not exist in fieldMap , it will be removed", sort.getField());
            return null;
        }
        return new SqlSort(jdbcColumn, sort.isAsc());
    }

    /**
     * 将一组 SQL 条件包装到 {@link AbstractHelper} 中.
     *
     * @param sqlHelper  目标 SQL 助手
     * @param conditions 待包装的 SQL 条件集合
     * @param symbol     连接这些条件的逻辑符号 (AND/OR)
     * @since 1.0.0
     */
    public static void warpConditions(AbstractHelper<?, ?> sqlHelper, Collection<Condition> conditions, String symbol) {
        if (sqlHelper == null || conditions == null || conditions.isEmpty()) {
            return;
        }
        symbol = SqlKeyword.replaceConnector(symbol);
        if (SqlKeyword.AND.getKeyword().equals(symbol)) {
            sqlHelper.getConditions().addAll(conditions);
            return;
        }
        SqlTree iSqlTrees = new SqlTree(conditions, SqlKeyword.OR.getKeyword());
        sqlHelper.merge(iSqlTrees);
    }

    /**
     * 将一组 SQL 排序规则包装到 {@link AbstractHelper} 中.
     *
     * @param sqlHelper                目标 SQL 助手
     * @param sorts                    待包装的 SQL 排序规则集合
     * @param propertyToColumnAliasMap 属性名到数据库列名的映射
     * @since 1.0.0
     */
    public static void wrapSorts(AbstractHelper<?, ?> sqlHelper, Collection<Sort> sorts, Map<String, String> propertyToColumnAliasMap) {
        for (Sort sort : sorts) {
            Sort validateSort = validateSort(sort, propertyToColumnAliasMap);
            if (validateSort != null) {
                sqlHelper.getSorts().add(validateSort);
            }
        }
    }

    /**
     * 处理并转换根 SQL 助手, 生成一个经过验证和映射的 SQL 助手.
     *
     * @param rootHelper 根 SQL 助手
     * @param <T>        实体类型
     * @return 处理后的 {@link BaseHelper} 实例
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     * @since 1.0.0
     */
    public static <T> BaseHelper<T> process(BaseHelper<T> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        SqlHelper<T> resultHelper = SqlHelper.of(entityClass);
        Map<String, String> propertyToColumnAliasMap = BoostUtils.getPropertyToColumnAliasMap(entityClass);
        Map<String, Object> extraParams = resultHelper.getExtra();
        for (Tree currentHelper : rootHelper) {
            Collection<Condition> currentHelperConditions = currentHelper.getConditions();
            Iterator<Condition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<Condition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                Condition condition = conditionIterator.next();
                Condition validate = BasicProcessor.validateCondition(condition, propertyToColumnAliasMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            BasicProcessor.warpConditions(resultHelper, validatedConditions, currentHelper.getConnector());
        }
        BasicProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), propertyToColumnAliasMap);
        return resultHelper;
    }

}