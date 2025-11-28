package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.Sort;
import io.github.luminion.sqlbooster.model.query.ConditionNode;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.builder.AbstractBuilder;
import io.github.luminion.sqlbooster.model.query.SqlContext;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * 默认 SQL 处理器.
 * <p>
 * 提供 SQL 条件的验证和处理功能, 包括字段映射验证、操作符验证等.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class BuildUtils {
    /**
     * 当前处理器实例使用的后缀到操作符的映射.
     */
    private final static Map<String, String> DEFAULT_SUFFIX_MAP = new HashMap<>();

    static {
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Ne", SqlKeyword.NE.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Lt", SqlKeyword.LT.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Lte", SqlKeyword.LTE.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Gt", SqlKeyword.GT.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Gte", SqlKeyword.GTE.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "Like", SqlKeyword.LIKE.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "NotLike", SqlKeyword.NOT_LIKE.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "In", SqlKeyword.IN.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "NotIn", SqlKeyword.NOT_IN.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "IsNull", SqlKeyword.IS_NULL.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "IsNotNull", SqlKeyword.IS_NOT_NULL.getKeyword());

        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "BitAny", SqlKeyword.BIT_ANY.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "BitAll", SqlKeyword.BIT_ALL.getKeyword());
        addBothCamelCaseAndUnderscore(DEFAULT_SUFFIX_MAP, "BitNone", SqlKeyword.BIT_NONE.getKeyword());
    }


    /**
     * 向map中同时添加驼峰式和下划线式后缀的映射
     *
     * @param map      映射关系表
     * @param suffix   后缀
     * @param operator 操作符
     * @since 1.0.0
     */
    private static void addBothCamelCaseAndUnderscore(Map<String, String> map, String suffix, String operator) {
        map.putIfAbsent(suffix, operator);
        String camelCase = TableInfoUtils.underscoreToCamelCase(suffix);
        map.putIfAbsent(camelCase, operator);
        String underscore = TableInfoUtils.camelCaseToUnderscore(suffix);
        map.putIfAbsent(underscore, operator);
    }


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
    public static Condition replaceCondition(Condition condition, Map<String, String> propertyToColumnAliasMap, Map<String, Object> extra) {
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
            log.debug("condition field [{}] requires value but value is null, it will be removed and put into paramMap", field);
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
            value = value.toString();
            if (!value.toString().contains("%")) {
                value = "%" + value + "%";
            }
        }
        condition.setField(jdbcColumn);
        condition.setOperator(operator);
        condition.setValue(value);
        return condition;
    }

    /**
     * 验证并处理单个 SQL 排序规则.
     *
     * @param sort                     待验证的 SQL 排序规则
     * @param propertyToColumnAliasMap 属性名到数据库列名的映射
     * @return 验证并处理后的 {@link Sort}, 如果排序字段无效则返回 null
     * @since 1.0.0
     */
    public static Sort replaceSort(Sort sort, Map<String, String> propertyToColumnAliasMap) {
        String jdbcColumn = propertyToColumnAliasMap.get(sort.getField());
        if (jdbcColumn == null) {
            log.warn("sort field [{}] not exist in fieldMap , it will be removed", sort.getField());
            return null;
        }
        return sort;
    }

    /**
     * 构建供xml使用的Helper
     *
     * @param rootHelper 根 SQL 助手
     * @param <T>        实体类型
     * @param <S>        helper类型
     * @return 处理后的实例
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     * @since 1.0.0
     */
    public static <T, S extends AbstractBuilder<T, S>> SqlContext<T> build(AbstractBuilder<T, S> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        S resultHelper = rootHelper.newInstance();
        Map<String, String> propertyToColumnAliasMap = TableInfoUtils.getPropertyToColumnAliasMap(entityClass);
        Map<String, Object> extraParams = resultHelper.getExtra();
        for (ConditionNode currentHelper : rootHelper) {
            Collection<Condition> currentHelperConditions = currentHelper.getConditions();
            Iterator<Condition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<Condition> replacedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                Condition condition = conditionIterator.next();
                Condition replaceCondition = BuildUtils.replaceCondition(condition, propertyToColumnAliasMap, extraParams);
                if (replaceCondition == null) {
                    continue;
                }
                replacedConditions.add(replaceCondition);
            }
            resultHelper.appendConditions(replacedConditions, currentHelper.getConnector());
        }
        for (Sort sort : rootHelper.getSorts()) {
            Sort validateSort = replaceSort(sort, propertyToColumnAliasMap);
            if (validateSort != null) {
                resultHelper.getSorts().add(validateSort);
            }
        }
        return resultHelper;
    }


    public static <T, S extends AbstractBuilder<T, S>> SqlContext<T> buildWithSuffix(AbstractBuilder<T, S> rootHelper, Map<String, String> suffixToOperatorMap) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        S resultHelper = rootHelper.newInstance();
        Map<String, Object> extraParams = resultHelper.getExtra();
        Map<String, String> propertyToColumnAliasMap = TableInfoUtils.getPropertyToColumnAliasMap(entityClass);
        Set<String> suffixes = suffixToOperatorMap.keySet();
        for (ConditionNode currentHelper : rootHelper) {
            Collection<Condition> currentHelperConditions = currentHelper.getConditions();
            Iterator<Condition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<Condition> replacedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                Condition condition = conditionIterator.next();
                String field = condition.getField();
                String jdbcColumn = propertyToColumnAliasMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix) && field.length() > suffix.length()) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffixToOperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            Condition suffixCondition = new Condition(sourceFiled, operator, condition.getValue());
                            Condition validateSuffixCondition = BuildUtils.replaceCondition(suffixCondition, propertyToColumnAliasMap, extraParams);
                            if (validateSuffixCondition == null) {
                                continue;
                            }
                            replacedConditions.add(validateSuffixCondition);
                            break;
                        }
                    }
                    if (isSuffixMatched) {
                        continue;
                    }
                }
                Condition replaceCondition = BuildUtils.replaceCondition(condition, propertyToColumnAliasMap, extraParams);
                if (replaceCondition == null) {
                    continue;
                }
                replacedConditions.add(replaceCondition);
            }
            resultHelper.appendConditions(replacedConditions, currentHelper.getConnector());
        }
        for (Sort sort : rootHelper.getSorts()) {
            Sort validateSort = BuildUtils.replaceSort(sort, propertyToColumnAliasMap);
            if (validateSort != null) {
                resultHelper.getSorts().add(validateSort);
            }
        }
        return resultHelper;
    }

    public static <T, S extends AbstractBuilder<T, S>> SqlContext<T> buildWithSuffix(AbstractBuilder<T, S> rootHelper) {
        return buildWithSuffix(rootHelper, DEFAULT_SUFFIX_MAP);
    } 

}