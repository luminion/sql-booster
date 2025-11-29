package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.enums.ConditionSuffix;
import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.ConditionSegment;
import io.github.luminion.sqlbooster.model.query.Sort;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * SQL 上下文构建工具。
 * <p>
 * 负责将原始的 SqlContext 转换为包含数据库列名、经过严格校验的标准 SqlContext。
 */
@Slf4j
public abstract class SqlContextUtils {

    private static volatile List<Map.Entry<String, String>> defaultSuffixes;

    static {
        Map<String, String> map = new HashMap<>();
        for (ConditionSuffix conditionSuffix : ConditionSuffix.values()) {
            map.put(conditionSuffix.getCamelCase(), conditionSuffix.getSymbol());
            map.put(conditionSuffix.getUnderscore(), conditionSuffix.getSymbol());
        }
        refreshDefaultSuffixes(map);
    }

    public static void refreshDefaultSuffixes(Map<String, String> suffixToOperatorMap) {
        List<Map.Entry<String, String>> list = new ArrayList<>(suffixToOperatorMap.entrySet());
        // 按长度倒序，防止短后缀（如 "In"）优先匹配长后缀（如 "NotIn"）
        list.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        defaultSuffixes = list;
    }

    // ==================== 公开构建入口 ====================

    /**
     * 仅执行基础构建 (映射已知字段，其余放入 params)。
     */
    public static <T> SqlContext<T> build(Class<T> entityClass, SqlContext<?> source) {
        return buildBase(entityClass, source);
    }

    /**
     * 执行基础构建，并使用默认规则解析字段后缀。
     */
    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source) {
        return buildWithSuffix(entityClass, source, null);
    }

    /**
     * 执行基础构建，并使用自定义后缀规则解析。
     */
    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source, Map<String, String> customSuffixMap) {
        SqlContext<T> context = buildBase(entityClass, source);
        return resolveSuffixes(context, entityClass, customSuffixMap);
    }

    // ==================== 核心步骤 1: 基础构建 ====================

    private static <T> SqlContext<T> buildBase(Class<T> entityClass, SqlContext<?> source) {
        if (entityClass == null) throw new IllegalArgumentException("Entity class cannot be null");

        SqlContext<T> result = new SqlContext<>();
        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);
        Map<String, Object> params = result.getParams();

        // 1. 处理条件
        for (ConditionSegment node : source) {
            LinkedHashSet<Condition> validConditions = new LinkedHashSet<>();

            for (Condition c : node.getConditions()) {
                String field = c.getField();
                String column = columnMap.get(field);

                if (column != null) {
                    // 字段在实体中存在，进行校验
                    Condition finalC = validateAndCreate(column, c.getOperator(), c.getValue());
                    if (finalC != null) {
                        validConditions.add(finalC);
                    } else {
                        // 校验失败，保留为动态参数供 XML 使用
                        log.debug("Direct condition field [{}] ignored: validation failed.", field);
                        params.put(field, c.getValue());
                    }
                } else {
                    // 字段在实体中不存在，直接放入 params
                    params.putIfAbsent(field, c.getValue());
                }
            }

            if (!validConditions.isEmpty()) {
                result.appendConditions(validConditions, node.getConnector());
            }
        }

        // 2. 处理排序
        processSorts(source.getSorts(), columnMap, result.getSorts());

        return result;
    }

    // ==================== 核心步骤 2: 后缀解析 ====================

    private static <T> SqlContext<T> resolveSuffixes(SqlContext<T> context, Class<T> entityClass, Map<String, String> customSuffixMap) {
        Map<String, Object> params = context.getParams();
        if (params.isEmpty()) {
            return context;
        }

        List<Map.Entry<String, String>> suffixList = (customSuffixMap != null && !customSuffixMap.isEmpty())
                ? new ArrayList<>(customSuffixMap.entrySet())
                : defaultSuffixes;
        if (customSuffixMap != null) {
            suffixList.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        }


        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);

        // 使用迭代器遍历并安全移除已解析的参数
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
                        String operator = suffixEntry.getValue();
                        Condition finalC = validateAndCreate(realColumn, operator, value);
                        if (finalC != null) {
                            // 匹配成功且校验通过 -> 移入 conditions
                            context.getConditions().add(finalC);
                            it.remove();
                        }
                        // 只要匹配到后缀，无论校验是否成功，都停止对当前字段匹配其他后缀
                        break;
                    }
                }
            }
        }
        return context;
    }

    // ==================== 辅助方法 ====================

    private static void processSorts(Set<Sort> sourceSorts, Map<String, String> columnMap, Set<Sort> targetSorts) {
        for (Sort sort : sourceSorts) {
            String column = columnMap.get(sort.getField());
            if (column != null) {
                targetSorts.add(new Sort(column, sort.isAsc()));
            } else {
                log.debug("Sort field [{}] ignored: unmapped.", sort.getField());
            }
        }
    }

    /**
     * 根据操作符和值进行校验，并创建合法的 Condition 对象。
     * <p>包含了所有内置操作符的严格类型检查规则。</p>
     */
    private static Condition validateAndCreate(String column, String operatorStr, Object value) {
        SqlKeyword keyword;
        try {
            keyword = SqlKeyword.resolve(operatorStr);
        } catch (IllegalArgumentException e) {
            return null; // 无法识别的操作符
        }

        // 规则: IS NULL / IS NOT NULL
        if (keyword.isNullCheck()) {
            // 只有当值为 true 时才生效
            return (value instanceof Boolean && (Boolean) value)
                    ? new Condition(column, keyword.getSymbol(), true)
                    : null;
        }

        // 规则: 其他所有操作符，值都不能为 null
        if (value == null) {
            return null;
        }

        // 规则: IN / NOT IN，值必须是有效的集合
        if (keyword.isIn()) {
            if (!(value instanceof Iterable) || !((Iterable<?>) value).iterator().hasNext()) {
                return null;
            }
        }

        // 规则: 位运算，值必须是整型
        if (keyword.isBitOperation()) {
            if (!(value instanceof Integer || value instanceof Long || value instanceof Short || value instanceof Byte)) {
                return null;
            }
        }

        // 规则: 大小比较，值必须可比较
        if (keyword.isCompare()) {
            if (!(value instanceof Comparable)) {
                return null;
            }
        }

        // 规则: LIKE，如果值不包含 '%'，自动添加 '%%'
        if (keyword.isLike()) {
            String s = value.toString();
            if (!s.contains("%")) {
                value = "%" + s + "%";
            }
        }

        return new Condition(column, keyword.getSymbol(), value);
    }
}
