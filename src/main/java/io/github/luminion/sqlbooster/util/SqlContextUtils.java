package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.ConditionSegment;
import io.github.luminion.sqlbooster.model.Sort;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

@Slf4j
public abstract class SqlContextUtils {

    private static volatile List<Map.Entry<String, String>> defaultSuffixes;

    static {
        Map<String, String> map = new HashMap<>();
        map.put("Ne", SqlKeyword.NE.getSymbol());
        map.put("_ne", SqlKeyword.NE.getSymbol());
        map.put("Lt", SqlKeyword.LT.getSymbol());
        map.put("_lt", SqlKeyword.LT.getSymbol());
        map.put("Lte", SqlKeyword.LTE.getSymbol());
        map.put("_lte", SqlKeyword.LTE.getSymbol());
        map.put("Le", SqlKeyword.LTE.getSymbol());
        map.put("_le", SqlKeyword.LTE.getSymbol());
        map.put("Gt", SqlKeyword.GT.getSymbol());
        map.put("_gt", SqlKeyword.GT.getSymbol());
        map.put("Gte", SqlKeyword.GTE.getSymbol());
        map.put("_gte", SqlKeyword.GTE.getSymbol());
        map.put("Ge", SqlKeyword.GTE.getSymbol());
        map.put("_ge", SqlKeyword.GTE.getSymbol());
        map.put("Like", SqlKeyword.LIKE.getSymbol());
        map.put("_like", SqlKeyword.LIKE.getSymbol());
        map.put("NotLike", SqlKeyword.NOT_LIKE.getSymbol());
        map.put("_not_like", SqlKeyword.NOT_LIKE.getSymbol());
        map.put("In", SqlKeyword.IN.getSymbol());
        map.put("_in", SqlKeyword.IN.getSymbol());
        map.put("NotIn", SqlKeyword.NOT_IN.getSymbol());
        map.put("_not_in", SqlKeyword.NOT_IN.getSymbol());
        map.put("IsNull", SqlKeyword.IS_NULL.getSymbol());
        map.put("_is_null", SqlKeyword.IS_NULL.getSymbol());
        map.put("IsNotNull", SqlKeyword.IS_NOT_NULL.getSymbol());
        map.put("_is_not_null", SqlKeyword.IS_NOT_NULL.getSymbol());
        map.put("HasAnyBits", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("_has_any_bits", SqlKeyword.HAS_ANY_BITS.getSymbol());
        map.put("HasAllBits", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("_has_all_bits", SqlKeyword.HAS_ALL_BITS.getSymbol());
        map.put("HasNoBits", SqlKeyword.HAS_NO_BITS.getSymbol());
        map.put("_has_no_bits", SqlKeyword.HAS_NO_BITS.getSymbol());
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
        // 后缀要按长度倒序匹配，避免 `Like` 抢先吃掉 `NotLike` 这类更长的规则。
        list.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        defaultSuffixes = list;
    }

    public static <T> SqlContext<T> copy(SqlContext<?> source) {
        SqlContext<T> result = new SqlContext<>();
        if (source == null) {
            return result;
        }
        for (Map.Entry<String, Object> entry : source.getParams().entrySet()) {
            result.getParams().put(entry.getKey(), copyValue(entry.getValue()));
        }
        for (Sort sort : source.getSorts()) {
            addSortDistinct(result.getSorts(), new Sort(sort.getField(), sort.isAsc()));
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
                        copyValue(condition.getValue())));
            }
            first = false;
        }
        return result;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static Object copyValue(Object value) {
        if (value == null) {
            return null;
        }
        // 复制时保留原容器的顺序/排序语义：有序 Set/Map 若一律降级成 HashSet/HashMap，
        // 依赖参数迭代顺序的调用方会拿到乱序副本。
        if (value instanceof SortedSet) {
            TreeSet<Object> copy = new TreeSet<>(((SortedSet<Object>) value).comparator());
            copy.addAll((SortedSet<Object>) value);
            return copy;
        }
        if (value instanceof Set) {
            return new LinkedHashSet<>((Collection<?>) value);
        }
        if (value instanceof Collection) {
            return new ArrayList<>((Collection<?>) value);
        }
        if (value instanceof SortedMap) {
            TreeMap<Object, Object> copy = new TreeMap<>(((SortedMap<Object, Object>) value).comparator());
            copy.putAll((SortedMap<Object, Object>) value);
            return copy;
        }
        if (value instanceof Map) {
            return new LinkedHashMap<>((Map<?, ?>) value);
        }
        if (value.getClass().isArray()) {
            int length = Array.getLength(value);
            Object copy = Array.newInstance(value.getClass().getComponentType(), length);
            System.arraycopy(value, 0, copy, 0, length);
            return copy;
        }
        return value;
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

    public static <T> SqlContext<T> normalize(Class<T> entityClass, SqlContext<?> source,
                                              Map<String, String> customSuffixMap,
                                              boolean includeDefaultSuffixes) {
        return buildWithSuffix(entityClass, source, customSuffixMap, includeDefaultSuffixes);
    }

    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source) {
        return buildContext(entityClass, source, defaultSuffixes);
    }

    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source,
                                                    Map<String, String> customSuffixMap) {
        return buildWithSuffix(entityClass, source, customSuffixMap, false);
    }

    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source,
                                                    Map<String, String> customSuffixMap,
                                                    boolean includeDefaultSuffixes) {
        return buildContext(entityClass, source, resolveSuffixEntries(customSuffixMap, includeDefaultSuffixes));
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
        // strict 模式下要求“输入多少条件/排序，输出就成功解析多少”，
        // 一旦有未识别字段或非法值就直接报错，而不是静默忽略。
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
        return buildContext(entityClass, source, null);
    }

    private static <T> SqlContext<T> buildContext(Class<T> entityClass, SqlContext<?> source,
                                                  List<Map.Entry<String, String>> suffixEntries) {
        if (entityClass == null) {
            throw new IllegalArgumentException("Entity class cannot be null");
        }

        SqlContext<T> result = new SqlContext<>();
        if (source == null) {
            return result;
        }

        result.getParams().putAll(source.getParams());
        // 先处理原始 params，再处理条件节点里的 field。
        // 这样 map/bean 输入和手工拼出的 SqlContext 都会走同一套后缀规则。
        resolveParamSuffixes(result, entityClass, suffixEntries);

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
                } else if (suffixEntries != null && !suffixEntries.isEmpty()) {
                    // 当前字段不是直接字段名时，再尝试按“字段名 + 后缀”拆解。
                    // 例如 `ageGe` -> `age` + `Ge`。
                    Condition finalCondition = resolveSuffixCondition(field, condition.getValue(), columnMap,
                            suffixEntries);
                    if (finalCondition != null) {
                        validConditions.add(finalCondition);
                    } else {
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

    private static List<Map.Entry<String, String>> resolveSuffixEntries(Map<String, String> customSuffixMap,
                                                                        boolean includeDefaultSuffixes) {
        if (customSuffixMap == null || customSuffixMap.isEmpty()) {
            return defaultSuffixes;
        }
        if (!includeDefaultSuffixes) {
            List<Map.Entry<String, String>> suffixEntries = new ArrayList<>(customSuffixMap.entrySet());
            suffixEntries.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
            return suffixEntries;
        }
        // 兼容模式：默认规则先铺底，再让自定义规则覆盖同名后缀。
        Map<String, String> merged = new HashMap<>();
        for (Map.Entry<String, String> entry : defaultSuffixes) {
            merged.put(entry.getKey(), entry.getValue());
        }
        merged.putAll(customSuffixMap);
        List<Map.Entry<String, String>> suffixEntries = new ArrayList<>(merged.entrySet());
        suffixEntries.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        return suffixEntries;
    }

    private static <T> SqlContext<T> resolveParamSuffixes(SqlContext<T> context, Class<T> entityClass,
                                                          List<Map.Entry<String, String>> suffixEntries) {
        Map<String, Object> params = context.getParams();
        if (params.isEmpty() || suffixEntries == null || suffixEntries.isEmpty()) {
            return context;
        }

        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);
        Iterator<Map.Entry<String, Object>> it = params.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<String, Object> entry = it.next();
            String field = entry.getKey();
            Object value = entry.getValue();

            Condition finalCondition = resolveSuffixCondition(field, value, columnMap, suffixEntries);
            if (finalCondition != null) {
                // 命中后缀规则后把它转回标准条件，并从 params 里移除，避免后面再次参与未识别字段判断。
                context.getConditions().add(finalCondition);
                it.remove();
            }
        }
        return context;
    }

    private static Condition resolveSuffixCondition(String field, Object value, Map<String, String> columnMap,
                                                    List<Map.Entry<String, String>> suffixEntries) {
        if (field == null || field.isEmpty() || suffixEntries == null || suffixEntries.isEmpty()) {
            return null;
        }
        for (Map.Entry<String, String> suffixEntry : suffixEntries) {
            String suffix = suffixEntry.getKey();
            if (field.endsWith(suffix) && field.length() > suffix.length()) {
                String realField = field.substring(0, field.length() - suffix.length());
                String realColumn = columnMap.get(realField);
                if (realColumn != null) {
                    return validateAndCreate(realColumn, suffixEntry.getValue(), value);
                }
            }
        }
        return null;
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
                addSortDistinct(targetSorts, new Sort(column, sort.isAsc()));
            } else {
                log.debug("Sort field [{}] ignored: unmapped.", sort.getField());
            }
        }
    }

    /**
     * 按字段名去重排序：同一字段只保留首个方向。
     * <p>
     * {@link Sort} 的 equals 含 asc，同字段的 ASC / DESC 会被当成两个不同元素同时进入
     * {@code LinkedHashSet}，渲染出 {@code ORDER BY a.f, a.f DESC} 这种自相矛盾的子句。
     */
    private static void addSortDistinct(Collection<Sort> targetSorts, Sort sort) {
        for (Sort existing : targetSorts) {
            if (existing.getField().equals(sort.getField())) {
                return;
            }
        }
        targetSorts.add(sort);
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
                // 用户大多数时候传的是裸值，这里统一补成模糊匹配。
                value = "%" + s + "%";
            }
        }

        return new Condition(column, keyword.getSymbol(), value);
    }
}
