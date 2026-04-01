package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.Condition;
import io.github.luminion.sqlbooster.model.ConditionSegment;
import io.github.luminion.sqlbooster.model.Sort;
import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import io.github.luminion.sqlbooster.util.SqlContextUtils;
import lombok.Getter;

import java.util.Map;
import java.util.function.BiFunction;

@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractSqlBuilder<T, S extends AbstractSqlBuilder<T, S>> {

    @Getter
    protected final Class<T> entityClass;
    protected final SqlContext<T> sqlContext = new SqlContext<>();

    protected AbstractSqlBuilder(Class<T> entityClass) {
        if (entityClass == null) {
            throw new IllegalArgumentException("entityClass cannot be null");
        }
        this.entityClass = entityClass;
    }

    protected abstract S newInstance();

    @Deprecated
    public SqlContext<T> build() {
        return toSqlContext();
    }

    @Deprecated
    public SqlContext<T> build(BiFunction<Class<T>, SqlContext<T>, SqlContext<T>> builder) {
        return builder.apply(this.entityClass, toSqlContext());
    }

    /**
     * 返回独立副本，避免调用方拿到内部引用后继续改动当前 builder 的状态。
     */
    public SqlContext<T> toSqlContext() {
        return SqlContextUtils.copy(this.sqlContext);
    }

    protected S addCondition(Condition condition) {
        mergeCondition(condition, null, false);
        return (S) this;
    }

    protected S addSort(Sort sort) {
        if (sort != null) {
            SqlContextUtils.appendSort(this.entityClass, this.sqlContext, sort, false);
        }
        return (S) this;
    }

    protected S mergeSegment(ConditionSegment conditionSegment) {
        mergeConditionSegment(conditionSegment, null, false);
        return (S) this;
    }

    public S append(Condition condition) {
        return addCondition(condition);
    }

    public S append(Condition condition, Map<String, String> customSuffixMap) {
        return append(condition, customSuffixMap, false);
    }

    public S append(Condition condition, Map<String, String> customSuffixMap, boolean includeDefaultSuffixes) {
        mergeCondition(condition, customSuffixMap, includeDefaultSuffixes);
        return (S) this;
    }

    public S append(Sort sort) {
        return addSort(sort);
    }

    public S append(ConditionSegment conditionSegment) {
        return mergeSegment(conditionSegment);
    }

    public S append(ConditionSegment conditionSegment, Map<String, String> customSuffixMap) {
        return append(conditionSegment, customSuffixMap, false);
    }

    public S append(ConditionSegment conditionSegment, Map<String, String> customSuffixMap,
                    boolean includeDefaultSuffixes) {
        mergeConditionSegment(conditionSegment, customSuffixMap, includeDefaultSuffixes);
        return (S) this;
    }

    public S where(Condition condition) {
        return addCondition(condition);
    }

    public S where(Condition condition, Map<String, String> customSuffixMap) {
        return where(condition, customSuffixMap, false);
    }

    public S where(Condition condition, Map<String, String> customSuffixMap, boolean includeDefaultSuffixes) {
        return append(condition, customSuffixMap, includeDefaultSuffixes);
    }

    public S orderBy(Sort sort) {
        return addSort(sort);
    }

    public S where(ConditionSegment conditionSegment) {
        return mergeSegment(conditionSegment);
    }

    public S where(ConditionSegment conditionSegment, Map<String, String> customSuffixMap) {
        return where(conditionSegment, customSuffixMap, false);
    }

    public S where(ConditionSegment conditionSegment, Map<String, String> customSuffixMap,
                   boolean includeDefaultSuffixes) {
        return append(conditionSegment, customSuffixMap, includeDefaultSuffixes);
    }

    public S fromMap(Map<?, ?> map) {
        return fromMap(map, null);
    }

    public S fromMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        return fromMap(map, customSuffixMap, false);
    }

    public S fromMap(Map<?, ?> map, Map<String, String> customSuffixMap, boolean includeDefaultSuffixes) {
        if (map != null && !map.isEmpty()) {
            // `includeDefaultSuffixes=true` 表示“在默认后缀规则上追加自定义规则”，
            // `false` 则保持旧行为：只按传入 map 里的后缀规则解析。
            mergeMapInput(map, customSuffixMap, includeDefaultSuffixes);
        }
        return (S) this;
    }

    public S fromBean(Object bean) {
        return fromBean(bean, null);
    }

    public S fromBean(Object bean, Map<String, String> customSuffixMap) {
        return fromBean(bean, customSuffixMap, false);
    }

    public S fromBean(Object bean, Map<String, String> customSuffixMap, boolean includeDefaultSuffixes) {
        if (bean != null) {
            mergeMapInput(BeanPropertyUtils.toMap(bean), customSuffixMap, includeDefaultSuffixes);
        }
        return (S) this;
    }

    @Deprecated
    public S appendByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    @Deprecated
    public S appendByMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        return fromMap(map, customSuffixMap);
    }

    @Deprecated
    public S appendByBean(Object bean) {
        return fromBean(bean);
    }

    @Deprecated
    public S appendByBean(Object bean, Map<String, String> customSuffixMap) {
        return fromBean(bean, customSuffixMap);
    }

    @Deprecated
    public S appendEqByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    @Deprecated
    public S appendEqByBean(Object bean) {
        return fromBean(bean);
    }

    private void mergeMapInput(Map<?, ?> map, Map<String, String> customSuffixMap, boolean includeDefaultSuffixes) {
        SqlContext<T> source = new SqlContext<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Object key = entry.getKey();
            Object value = entry.getValue();
            if (key != null && value != null) {
                source.getConditions().add(new Condition(key.toString(), SqlKeyword.EQ.getSymbol(), value));
            }
        }
        // Map / Bean 输入先统一转成 EQ 条件，再在 SqlContextUtils 里做字段映射和后缀解析，
        // 这样各种入口最后都走同一套规范化逻辑。
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap,
                includeDefaultSuffixes));
    }

    private void mergeCondition(Condition condition, Map<String, String> customSuffixMap,
                                boolean includeDefaultSuffixes) {
        if (condition == null) {
            return;
        }
        SqlContext<T> source = new SqlContext<>();
        source.getConditions().add(new Condition(condition.getField(), condition.getOperator(), condition.getValue()));
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap,
                includeDefaultSuffixes));
    }

    private void mergeConditionSegment(ConditionSegment conditionSegment, Map<String, String> customSuffixMap,
                                       boolean includeDefaultSuffixes) {
        if (conditionSegment == null) {
            return;
        }
        SqlContext<T> source = new SqlContext<>();
        source.merge(conditionSegment);
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap,
                includeDefaultSuffixes));
    }
}
