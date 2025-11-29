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
 * SQL 上下文构建工具.
 * <p>
 * 负责将原始的 SqlContext 转换为包含数据库列名、经过严格校验的标准 SqlContext.
 *
 * @author luminion
 * @since 1.0.0
 */
@Slf4j
public abstract class SqlContextUtils {

    private static volatile List<Map.Entry<String, String>> defaultSuffixes;

    static {
        Map<String, String> map = new HashMap<>();
        for (ConditionSuffix conditionSuffix : ConditionSuffix.values()) {
            map.put(conditionSuffix.getCamelCase(), conditionSuffix.getKeyword());
            map.put(conditionSuffix.getUnderscore(), conditionSuffix.getKeyword());
        }
        refreshDefaultSuffixes(map);
    }

    public static void refreshDefaultSuffixes(Map<String, String> suffixToOperatorMap) {
        List<Map.Entry<String, String>> list = new ArrayList<>(suffixToOperatorMap.entrySet());
        // 按长度倒序，防止短后缀截断长后缀
        list.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        defaultSuffixes = list;
    }

    // ==================== 公开构建入口 ====================

    /**
     * 仅执行基础构建 (映射已知字段，其余放入 params).
     */
    public static <T> SqlContext<T> build(Class<T> entityClass, SqlContext<?> source) {
        return buildBase(entityClass, source);
    }

    /**
     * 执行基础构建 + 后缀解析.
     */
    public static <T> SqlContext<T> buildWithSuffix(Class<T> entityClass, SqlContext<?> source) {
        return buildWithSuffix( entityClass,source, null);
    }

    /**
     * 执行基础构建 + 自定义后缀解析.
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
                    // 命中直接映射 -> 尝试校验
                    Condition finalC = validateAndCreate(column, c.getOperator(), c.getValue());
                    if (finalC != null) {
                        validConditions.add(finalC);
                    } else {
                        // 校验失败，保留参数供 xml 使用 (或者你可以选择 drop 掉)
                        log.debug("Direct condition field [{}] ignored: validation failed.", field);
                        params.put(field, c.getValue());
                    }
                } else {
                    // 未命中映射 -> 直接放入 params
                    params.putIfAbsent(field, c.getValue());
                }
            }

            if (!validConditions.isEmpty()) {
                result.appendConditions(validConditions, node.getConnector());
            }
        }

        // 2. 处理排序 (抽离后的方法)
        processSorts(source.getSorts(), columnMap, result.getSorts());

        return result;
    }

    // ==================== 核心步骤 2: 后缀解析 ====================

    private static <T> SqlContext<T> resolveSuffixes(SqlContext<T> context, Class<T> entityClass, Map<String, String> customSuffixMap) {
        Map<String, Object> params = context.getParams();
        if (params.isEmpty()) {
            return context;
        }

        List<Map.Entry<String, String>> suffixList;
        if (customSuffixMap != null && !customSuffixMap.isEmpty()) {
            suffixList = new ArrayList<>(customSuffixMap.entrySet());
            suffixList.sort((a, b) -> Integer.compare(b.getKey().length(), a.getKey().length()));
        } else {
            suffixList = defaultSuffixes;
        }

        Map<String, String> columnMap = TableMetaRegistry.getPropertyToColumnAliasMap(entityClass);

        // 使用迭代器遍历并安全移除
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
                        // 只要匹配到后缀，无论校验是否成功，都停止匹配其他后缀
                        break;
                    }
                }
            }
        }
        return context;
    }

    // ==================== 辅助方法 ====================

    /**
     * 处理排序字段.
     */
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
     * 通用校验与创建逻辑.
     * <p>包含严格的类型检查规则.</p>
     */
    private static Condition validateAndCreate(String column, String operatorStr, Object value) {
        // 1. 操作符解析
        SqlKeyword keyword;
        try {
            keyword = SqlKeyword.resolve(operatorStr);
        } catch (IllegalArgumentException e) {
            return null;
        }

        // 2. Null 操作符特殊处理 (IS NULL / IS NOT NULL)
        if (keyword.isNullCheck()) {
            if (value instanceof Boolean && (Boolean) value) {
                // IS NULL 不需要值，这里传 value 仅仅是为了保持对象完整，生成 SQL 时应忽略该值
                return new Condition(column, keyword.getSymbol(), value);
            }
            return null;
        }

        // 3. 其他操作符不允许 value 为 null
        if (value == null) {
            return null;
        }

        // 4. 集合校验 (IN / NOT IN)
        if (keyword.isIn()) {
            if (!(value instanceof Iterable) || !((Iterable<?>) value).iterator().hasNext()) {
                return null;
            }
        }

        // 5. 位运算校验 (BIT)
        // 规则：入参必须是整型 (Byte, Short, Integer, Long)
        if (keyword.isBitOperation()) {
            if (!(value instanceof Integer || value instanceof Long || value instanceof Short || value instanceof Byte)) {
                return null;
            }
        }

        // 6. 比较运算校验 (GT, GE, LT, LE, NE)
        // 规则：入参必须实现了 Comparable 接口 (如 Integer, String, Date)
        if (keyword.isCompare()) {
            if (!(value instanceof Comparable)) {
                return null;
            }
        }

        // 7. LIKE 处理
        if (keyword.isLike()) {
            String s = value.toString();
            if (!s.contains("%")) {
                value = "%" + s + "%";
            }
        }

        return new Condition(column, keyword.getSymbol(), value);
    }
}