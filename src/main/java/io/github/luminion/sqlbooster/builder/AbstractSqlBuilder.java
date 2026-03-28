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

/**
 * SQL 构建器基类。
 *
 * @param <T> 实体类型
 * @param <S> 构建器类型
 */
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

    /**
     * 构建查询上下文。
     *
     * @deprecated 请使用 {@link #toSqlContext()}
     */
    @Deprecated
    public SqlContext<T> build() {
        return toSqlContext();
    }

    /**
     * 构建查询上下文。
     *
     * @deprecated 请使用 {@link #toSqlContext()}
     */
    @Deprecated
    public SqlContext<T> build(BiFunction<Class<T>, SqlContext<T>, SqlContext<T>> builder) {
        return builder.apply(this.entityClass, toSqlContext());
    }

    /**
     * 转换为独立的 SqlContext 副本。
     */
    public SqlContext<T> toSqlContext() {
        return SqlContextUtils.copy(this.sqlContext);
    }

    protected S addCondition(Condition condition) {
        mergeCondition(condition, null);
        return (S) this;
    }

    protected S addSort(Sort sort) {
        if (sort != null) {
            SqlContextUtils.appendSort(this.entityClass, this.sqlContext, sort, false);
        }
        return (S) this;
    }

    protected S mergeSegment(ConditionSegment conditionSegment) {
        mergeConditionSegment(conditionSegment, null);
        return (S) this;
    }

    public S append(Condition condition) {
        return addCondition(condition);
    }

    public S append(Condition condition, Map<String, String> customSuffixMap) {
        mergeCondition(condition, customSuffixMap);
        return (S) this;
    }

    public S append(Sort sort) {
        return addSort(sort);
    }

    public S append(ConditionSegment conditionSegment) {
        return mergeSegment(conditionSegment);
    }

    public S append(ConditionSegment conditionSegment, Map<String, String> customSuffixMap) {
        mergeConditionSegment(conditionSegment, customSuffixMap);
        return (S) this;
    }

    public S where(Condition condition) {
        return addCondition(condition);
    }

    public S where(Condition condition, Map<String, String> customSuffixMap) {
        return append(condition, customSuffixMap);
    }

    public S orderBy(Sort sort) {
        return addSort(sort);
    }

    public S where(ConditionSegment conditionSegment) {
        return mergeSegment(conditionSegment);
    }

    public S where(ConditionSegment conditionSegment, Map<String, String> customSuffixMap) {
        return append(conditionSegment, customSuffixMap);
    }

    public S fromMap(Map<?, ?> map) {
        return fromMap(map, null);
    }

    public S fromMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        if (map != null && !map.isEmpty()) {
            mergeMapInput(map, customSuffixMap);
        }
        return (S) this;
    }

    public S fromBean(Object bean) {
        return fromBean(bean, null);
    }

    public S fromBean(Object bean, Map<String, String> customSuffixMap) {
        if (bean != null) {
            mergeMapInput(BeanPropertyUtils.toMap(bean), customSuffixMap);
        }
        return (S) this;
    }

    /**
     * @deprecated 请使用 {@link #fromMap(Map)}
     */
    @Deprecated
    public S appendByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    /**
     * @deprecated 请使用 {@link #fromMap(Map, Map)}
     */
    @Deprecated
    public S appendByMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        return fromMap(map, customSuffixMap);
    }

    /**
     * @deprecated 请使用 {@link #fromBean(Object)}
     */
    @Deprecated
    public S appendByBean(Object bean) {
        return fromBean(bean);
    }

    /**
     * @deprecated 请使用 {@link #fromBean(Object, Map)}
     */
    @Deprecated
    public S appendByBean(Object bean, Map<String, String> customSuffixMap) {
        return fromBean(bean, customSuffixMap);
    }

    /**
     * @deprecated 请使用 {@link #fromMap(Map)}
     */
    @Deprecated
    public S appendEqByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    /**
     * @deprecated 请使用 {@link #fromBean(Object)}
     */
    @Deprecated
    public S appendEqByBean(Object bean) {
        return fromBean(bean);
    }

    private void mergeMapInput(Map<?, ?> map, Map<String, String> customSuffixMap) {
        SqlContext<T> source = new SqlContext<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Object key = entry.getKey();
            Object value = entry.getValue();
            if (key != null && value != null) {
                source.getConditions().add(new Condition(key.toString(), SqlKeyword.EQ.getSymbol(), value));
            }
        }
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap));
    }

    private void mergeCondition(Condition condition, Map<String, String> customSuffixMap) {
        if (condition == null) {
            return;
        }
        SqlContext<T> source = new SqlContext<>();
        source.getConditions().add(new Condition(condition.getField(), condition.getOperator(), condition.getValue()));
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap));
    }

    private void mergeConditionSegment(ConditionSegment conditionSegment, Map<String, String> customSuffixMap) {
        if (conditionSegment == null) {
            return;
        }
        SqlContext<T> source = new SqlContext<>();
        source.merge(conditionSegment);
        this.sqlContext.merge(SqlContextUtils.buildWithSuffix(this.entityClass, source, customSuffixMap));
    }
}