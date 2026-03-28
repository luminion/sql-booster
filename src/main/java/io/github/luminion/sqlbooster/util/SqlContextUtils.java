package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.ConditionSegment;
import io.github.luminion.sqlbooster.model.Sort;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

/**
 * SQL 上下文构建工具。
 */
@Slf4j
public abstract class SqlContextUtils {

    private static volatile List<Map.Entry<String, String>> defaultSuffixes;

    static {
        Map<String, String> map = new HashMap<>();
        // 比较运算符
        map.put("Ne", SqlKeyword.NE.getSymbol());
        map.put("_ne", SqlKeyword.NE.getSymbol());
        map.put("Lt", SqlKeyword.LT.getSymbol());
        map.put("_lt", SqlKeyword.LT.getSymbol());
        map.put("Lte", SqlKeyword.LTE.getSymbol());
        map.put("_lte", SqlKeyword.LTE.getSymbol());
        map.put("Gt", SqlKeyword.GT.getSymbol());
        map.put("_gt", SqlKeyword.GT.getSymbol());
        map.put("Gte", SqlKeyword.GTE.getSymbol());
        map.put("_gte", SqlKeyword.GTE.getSymbol());
        // 模糊匹配
        map.put("Like", SqlKeyword.LIKE.getSymbol());
        map.put("_like", SqlKeyword.LIKE.getSymbol());
        map.put("NotLike", SqlKeyword.NOT_LIKE.getSymbol());
        map.put("_not_like", SqlKeyword.NOT_LIKE.getSymbol());
        // 集合查询
        map.put("In", SqlKeyword.IN.getSymbol());
        map.put("_in", SqlKeyword.IN.getSymbol());
        map.put("NotIn", SqlKeyword.NOT_IN.getSymbol());
        map.put("_not_in", SqlKeyword.NOT_IN.getSymbol());
        // Null 判断
        map.put("IsNull", SqlKeyword.IS_NULL.getSymbol());
        map.put("_is_null", SqlKeyword.IS_NULL.getSymbol());
        map.put("IsNotNull", SqlKeyword.IS_NOT_NULL.getSymbol());
        map.put("_is_not_null", SqlKeyword.IS_NOT_NULL.getSymbol());
        // 位运算
        map.put("HasAnyBits", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("_has_any_bits", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("HasAllBits", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("_has_all_bits", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("HasNoBits", SqlKeyword.HAS_NO_BITS.getSymbol());
        map.put("_has_no_bits", SqlKeyword.HAS_NO_BITS.getSymbol());
        // 兼容旧后缀
        map.put("BitAny", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("_bit_any", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("BitAll", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("_bit_all", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("BitNone", SqlKeyword.HAS_NO_BITS.getSymbol());
        map.put("_bit_none", SqlKeyword.HAS_NO_BITS.getSymbol());
        refreshDefaultSuffixes(map);
    }

    public static void refreshDefaultSuffixes(Map<String, String> suffixToOperatorMap) {
        List<Map.Entry<String, String>> list = new ArrayList<>(suffixToOperatorMap.entrySet());
        list.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        defaultSuffixes = list;
    }

    public static <T> SqlContext<T> copy(SqlContext<?> source) {
        SqlContext<T> result = new SqlContext<>();
        if (source == null) {
            return result;
        }
        result.getParams().putAll(source.getParams());
        for (Sort sort : source.getSorts()) {
            result.getSorts().add(new Sort(sort.getField(), sort.isAsc()));
        }
        ConditionSegment target = result;
        boolean first = true;
        for (ConditionSegment node : source) {
            if (!first) {
                ConditionSegment next = new ConditionSegment();
                target.setNext(next);
                target = next;
            }
            target.setAnd(node.isAnd());
            for (Condition condition : node.getConditions()) {
                target.getConditions().add(new Condition(condition.getField(), condition.getOperator(),
                        condition.getValue()));
            }
            first = false;
        }
        return result;
    }

    public static <T> SqlContext<T> build(Class<T> entityClass, SqlContext<?> source) {
        return buildBase(entityClass, source);
    }

    public static <T> SqlContext<T> normalize(Class<T> entityClass, SqlContext<?> source) {
        return buildWithSuffix(entityClass, source, null);
    }

    public static <T> SqlContext<T> normalize(Class<T> entityClass, SqlContext<?> source,
                                              Map<String, String> customSuffixMap) {
        return buildWithSuffix(entityClass, source, customSuffixMap);
    }

    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source) {
        return buildWithSuffix(entityClass, source, null);
    }

    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source,
                                                    Map<String, String> customSuffixMap) {
        SqlContext<T> context = buildBase(entityClass, source);
        return resolveSuffixes(context, entityClass, customSuffixMap);
    }

    public static <T> SqlContext<T> appendByMap(Class<T> entityClass, SqlContext<T> target, Map<?, ?> map) {
        if (target == null || map == null || map.isEmpty()) {
            return target;
        }
        SqlContext<T> source = new SqlContext<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Object key = entry.getKey();
            Object value = entry.getValue();
            if (key != null && value != null) {
                source.getConditions().add(new Condition(key.toString(), SqlKeyword.EQ.getSymbol(), value));
            }
        }
        target.merge(buildWithSuffix(entityClass, source));
        return target;
    }

    public static <T> SqlContext<T> appendByBean(Class<T> entityClass, SqlContext<T> target, Object bean) {
        if (bean == null) {
            return target;
        }
        return appendByMap(entityClass, target, BeanPropertyUtils.toMap(bean));
    }

    public static <T> SqlContext<T> appendCondition(Class<T> entityClass, SqlContext<T> target, Condition condition,
                                                    boolean strict) {
        if (target == null || condition == null) {
            return target;
        }
        SqlContext<T> source = new SqlContext<>();
        source.getConditions().add(new Condition(condition.getField(), condition.getOperator(), condition.getValue()));
        SqlContext<T> normalized = buildWithSuffix(entityClass, source);
        return mergeValidated(entityClass, target, source, normalized, strict, "condition");
    }

    public static <T> SqlContext<T> appendSort(Class<T> entityClass, SqlContext<T> target, Sort sort,
                                               boolean strict) {
        if (target == null || sort == null) {
            return target;
        }
        SqlContext<T> source = new SqlContext<>();
        source.getSorts().add(new Sort(sort.getField(), sort.isAsc()));
        SqlContext<T> normalized = buildBase(entityClass, source);
        return mergeValidated(entityClass, target, source, normalized, strict, "sort");
    }

    public static <T> SqlContext<T> appendSegment(Class<T> entityClass, SqlContext<T> target,
                                                  ConditionSegment conditionSegment, boolean strict) {
        if (target == null || conditionSegment == null) {
            return target;
        }
        SqlContext<T> source = new SqlContext<>();
        source.merge(conditionSegment);
        SqlContext<T> normalized = buildWithSuffix(entityClass, source);
        return mergeValidated(entityClass, target, source, normalized, strict, "segment");
    }

    private static <T> SqlContext<T> mergeValidated(Class<T> entityClass, SqlContext<T> target, SqlContext<T> source,
                                                    SqlContext<T> normalized, boolean strict, String label) {
        if (!strict) {
            target.merge(normalized);
            return target;
        }
        int sourceConditionCount = countConditions(source);
        int normalizedConditionCount = countConditions(normalized);
        if (normalizedConditionCount != sourceConditionCount
                || normalized.getSorts().size() != source.getSorts().size()
                || !normalized.getParams().isEmpty()) {
            throw new IllegalArgumentException("Illegal " + label + " for entity [" + entityClass.getName()
                    + "], unresolved fields: " + normalized.getParams().keySet());
        }
        target.merge(normalized);
        return target;
    }

    private static int countConditions(ConditionSegment segment) {
        if (segment == null) {
            return 0;
        }
        int count = 0;
        for (ConditionSegment node : segment) {
            count += node.getConditions().size();
        }
        return count;
    }

    private static <T> SqlContext<T> buildBase(Class<T> entityClass, SqlContext<?> source) {
        if (entityClass == null) {
            throw new IllegalArgumentException("Entity class cannot be null");
        }

        SqlContext<T> result = new SqlContext<>();
        if (source == null) {
            return result;
        }

        result.getParams().putAll(source.getParams());

        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);
        Map<String, Object> params = result.getParams();
        Collection<String> columns = columnMap.values();

        for (ConditionSegment conditionSegment : source) {
            LinkedHashSet<Condition> validConditions = new LinkedHashSet<>();

            for (Condition condition : conditionSegment.getConditions()) {
                String field = condition.getField();
                if (field == null || field.isEmpty()) {
                    continue;
                }
                String column = columnMap.get(field);
                if (column == null && columns.contains(field)) {
                    column = field;
                }
                if (column != null) {
                    Condition finalCondition = validateAndCreate(column, condition.getOperator(), condition.getValue());
                    if (finalCondition != null) {
                        validConditions.add(finalCondition);
                    } else {
                        log.debug("Direct condition field [{}] ignored: validation failed.", field);
                        params.putIfAbsent(field, condition.getValue());
                    }
                } else {
                    params.putIfAbsent(field, condition.getValue());
                }
            }

            if (!validConditions.isEmpty()) {
                result.appendConditions(validConditions, conditionSegment.isAnd());
            }
        }

        processSorts(source.getSorts(), columnMap, result.getSorts());
        return result;
    }

    private static <T> SqlContext<T> resolveSuffixes(SqlContext<T> context, Class<T> entityClass,
                                                     Map<String, String> customSuffixMap) {
        Map<String, Object> params = context.getParams();
        if (params.isEmpty()) {
            return context;
        }

        List<Map.Entry<String, String>> suffixList = (customSuffixMap != null && !customSuffixMap.isEmpty())
                ? new ArrayList<>(customSuffixMap.entrySet())
                : defaultSuffixes;
        if (customSuffixMap != null && !customSuffixMap.isEmpty()) {
            suffixList.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        }

        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);
        Iterator<Map.Entry<String, Object>> it = params.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, Object> entry = it.next();
            String field = entry.getKey();
            Object value = entry.getValue();

            for (Map.Entry<String, String> suffixEntry : suffixList) {
                String suffix = suffixEntry.getKey();
                if (field.endsWith(suffix) && field.length() > suffix.length()) {
                    String realField = field.substring(0, field.length() - suffix.length());
                    String realColumn = columnMap.get(realField);
                    if (realColumn != null) {
                        Condition finalCondition = validateAndCreate(realColumn, suffixEntry.getValue(), value);
                        if (finalCondition != null) {
                            context.getConditions().add(finalCondition);
                            it.remove();
                        }
                        break;
                    }
                }
            }
        }
        return context;
    }

    private static void processSorts(Collection<Sort> sourceSorts, Map<String, String> columnMap,
                                     Collection<Sort> targetSorts) {
        Collection<String> columns = columnMap.values();
        for (Sort sort : sourceSorts) {
            String field = sort.getField();
            String column = columnMap.get(field);
            if (column == null && columns.contains(field)) {
                column = field;
            }
            if (column != null) {
                targetSorts.add(new Sort(column, sort.isAsc()));
            } else {
                log.debug("Sort field [{}] ignored: unmapped.", sort.getField());
            }
        }
    }

    private static Condition validateAndCreate(String column, String operatorStr, Object value) {
        SqlKeyword keyword;
        try {
            keyword = SqlKeyword.resolve(operatorStr);
        } catch (IllegalArgumentException e) {
            return null;
        }

        if (keyword.isNullCheck()) {
            return (value instanceof Boolean && (Boolean) value)
                    ? new Condition(column, keyword.getSymbol(), true)
                    : null;
        }

        if (value == null) {
            return null;
        }

        if (keyword.isIn()) {
            boolean isValid = false;
            if (value instanceof Iterable) {
                isValid = ((Iterable<?>) value).iterator().hasNext();
            } else if (value.getClass().isArray()) {
                isValid = java.lang.reflect.Array.getLength(value) > 0;
            }
            if (!isValid) {
                return null;
            }
        }

        if (keyword.isBitOperation()) {
            if (!(value instanceof Integer || value instanceof Long || value instanceof Short
                    || value instanceof Byte)) {
                return null;
            }
        }

        if (keyword.isCompare()) {
            if (!(value instanceof Comparable)) {
                return null;
            }
        }

        if (keyword.isLike()) {
            String s = value.toString();
            if (!s.contains("%")) {
                value = "%" + s + "%";
            }
        }

        return new Condition(column, keyword.getSymbol(), value);
    }
}
