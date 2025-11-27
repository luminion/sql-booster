package io.github.luminion.sqlbooster.model.helper.processor;

import io.github.luminion.sqlbooster.model.api.Condition;
import io.github.luminion.sqlbooster.model.api.ConditionNode;
import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.helper.AbstractHelper;
import io.github.luminion.sqlbooster.util.BoostUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * 字段后缀处理器.
 * <p>
 * 提供基于字段后缀的 SQL 条件处理功能, 支持通过字段后缀自动识别 SQL 操作符.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public class SuffixProcessor {
    /**
     * 默认的后缀到操作符的映射表.
     */
    private static Map<String, String> defaultSuffixToOperatorMap;
    /**
     * 当前处理器实例使用的后缀到操作符的映射.
     */
    private final Map<String, String> suffixToOperatorMap;

    /**
     * 设置默认的后缀到操作符的映射表.
     *
     * @param suffixToOperatorMap 后缀到操作符的映射表
     * @since 1.0.0
     */
    public static void defaultSuffixMap(Map<String, String> suffixToOperatorMap) {
        SuffixProcessor.defaultSuffixToOperatorMap = Collections.unmodifiableMap(suffixToOperatorMap);
    }

    static {
        HashMap<String, String> map = new HashMap<>();
        addBothCamelCaseAndUnderscore(map, "Ne", SqlKeyword.NE.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Lt", SqlKeyword.LT.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Lte", SqlKeyword.LTE.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Gt", SqlKeyword.GT.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Gte", SqlKeyword.GTE.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Like", SqlKeyword.LIKE.getKeyword());
        addBothCamelCaseAndUnderscore(map, "NotLike", SqlKeyword.NOT_LIKE.getKeyword());
        addBothCamelCaseAndUnderscore(map, "In", SqlKeyword.IN.getKeyword());
        addBothCamelCaseAndUnderscore(map, "NotIn", SqlKeyword.NOT_IN.getKeyword());
        addBothCamelCaseAndUnderscore(map, "Null", SqlKeyword.IS_NULL.getKeyword());
        addBothCamelCaseAndUnderscore(map, "IsNull", SqlKeyword.IS_NULL.getKeyword());
        addBothCamelCaseAndUnderscore(map, "NotNull", SqlKeyword.IS_NOT_NULL.getKeyword());
        addBothCamelCaseAndUnderscore(map, "IsNotNull", SqlKeyword.IS_NOT_NULL.getKeyword());
        addBothCamelCaseAndUnderscore(map, "BitAny", SqlKeyword.BIT_ANY.getKeyword());
        addBothCamelCaseAndUnderscore(map, "BitAll", SqlKeyword.BIT_ALL.getKeyword());
        addBothCamelCaseAndUnderscore(map, "BitNone", SqlKeyword.BIT_NONE.getKeyword());
        defaultSuffixToOperatorMap = Collections.unmodifiableMap(map);
    }

    /**
     * 向map中同时添加驼峰式和下划线式后缀的映射
     *
     * @param map      映射关系表
     * @param suffix   后缀
     * @param operator 操作符
     * @since 1.0.0
     */
    public static void addBothCamelCaseAndUnderscore(Map<String, String> map, String suffix, String operator) {
        map.putIfAbsent(suffix, operator);
        String camelCase = BoostUtils.underscoreToCamelCase(suffix);
        map.putIfAbsent(camelCase, operator);
        String underscore = BoostUtils.camelCaseToUnderscore(suffix);
        map.putIfAbsent(underscore, operator);
    }


    /**
     * 私有构造函数, 使用自定义的后缀映射.
     *
     * @param suffixToOperatorMap 自定义的后缀到操作符的映射
     */
    private SuffixProcessor(Map<String, String> suffixToOperatorMap) {
        if (suffixToOperatorMap == null) {
            throw new IllegalArgumentException("suffix2OperatorMap can't be null");
        }
        this.suffixToOperatorMap = suffixToOperatorMap;
    }


    /**
     * 获取默认的 {@link SuffixProcessor}
     *
     * @return 单例实例
     * @since 1.0.0
     */
    public static SuffixProcessor of() {
        return new SuffixProcessor(defaultSuffixToOperatorMap);
    }

    /**
     * 创建一个新的 {@link SuffixProcessor} 实例, 使用自定义的后缀映射.
     *
     * @param suffix2OperatorMap 自定义的后缀到操作符的映射
     * @return 新的 {@link SuffixProcessor} 实例
     * @since 1.0.0
     */
    public static SuffixProcessor of(Map<String, String> suffix2OperatorMap) {
        return new SuffixProcessor(suffix2OperatorMap);
    }

    /**
     * 处理 SQL 助手, 将字段后缀转换为对应的 SQL 操作符.
     *
     * @param rootHelper 根 SQL 助手
     * @param <T>        实体类型
     * @param <S>        helper类型
     * @return 处理后的实例
     * @throws IllegalArgumentException 当无法获取实体类时抛出
     * @since 1.0.0
     */
    public <T, S extends AbstractHelper<T, S>> S process(AbstractHelper<T, S> rootHelper) {
        Class<T> entityClass = rootHelper.getEntityClass();
        if (entityClass == null) {
            throw new IllegalArgumentException("can't get entity class from sql helper");
        }
        S resultHelper = rootHelper.newInstance();
        Map<String, Object> extraParams = resultHelper.getExtra();
        Map<String, String> entityPropertyToColumnAliasMap = BoostUtils.getPropertyToColumnAliasMap(entityClass);
        Set<String> suffixes = suffixToOperatorMap.keySet();
        for (ConditionNode currentHelper : rootHelper) {
            Collection<Condition> currentHelperConditions = currentHelper.getConditions();
            Iterator<Condition> conditionIterator = currentHelperConditions.iterator();
            LinkedHashSet<Condition> validatedConditions = new LinkedHashSet<>(currentHelperConditions.size());
            while (conditionIterator.hasNext()) {
                Condition condition = conditionIterator.next();
                String field = condition.getField();
                String jdbcColumn = entityPropertyToColumnAliasMap.get(field);
                if (jdbcColumn == null) {
                    boolean isSuffixMatched = false;
                    for (String suffix : suffixes) {
                        if (field.endsWith(suffix) && field.length() > suffix.length()) {
                            isSuffixMatched = true;
                            String sourceFiled = field.substring(0, field.length() - suffix.length());
                            String operator = suffixToOperatorMap.get(suffix);
                            log.debug("condition field [{}] Matched suffix operator [{}]", field, operator);
                            Condition suffixCondition = new Condition(sourceFiled, operator, condition.getValue());
                            Condition validateSuffixCondition = BasicProcessor.validateCondition(suffixCondition, entityPropertyToColumnAliasMap, extraParams);
                            if (validateSuffixCondition == null) {
                                continue;
                            }
                            validatedConditions.add(validateSuffixCondition);
                            break;
                        }
                    }
                    if (isSuffixMatched) {
                        continue;
                    }
                }
                Condition validate = BasicProcessor.validateCondition(condition, entityPropertyToColumnAliasMap, extraParams);
                if (validate == null) {
                    continue;
                }
                validatedConditions.add(validate);
            }
            resultHelper.appendConditions(validatedConditions, currentHelper.getConnector());
        }
        BasicProcessor.wrapSorts(resultHelper, rootHelper.getSorts(), entityPropertyToColumnAliasMap);
        return resultHelper;
    }

}