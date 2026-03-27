package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.ConditionSegment;
import io.github.luminion.sqlbooster.model.query.Sort;
import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import io.github.luminion.sqlbooster.util.SqlContextUtils;
import lombok.Getter;

import java.util.Map;
import java.util.function.BiFunction;

/**
 * Base class for SQL builders.
 *
 * @param <T> entity type
 * @param <S> builder type
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
     * 建造
     *
     * @deprecated use {@link #toSqlContext()} instead
     */
    public SqlContext<T> build() {
        return toSqlContext();
    }

    /**
     * 建造
     *
     * @deprecated use {@link #toSqlContext()} instead
     */
    public SqlContext<T> build(BiFunction<Class<T>, SqlContext<T>, SqlContext<T>> builder) {
        return builder.apply(this.entityClass, toSqlContext());
    }

    /**
     * 转化为SqlContext
     *
     * @since 1.2.0
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

    /**
     * add map key-value as conditions
     *
     * @since 1.2
     *
     */
    public S fromMap(Map<?, ?> map) {
        return fromMap(map, null);
    }

    /**
     * add map key-value as conditions
     *
     * @since 1.2
     *
     */
    public S fromMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        if (map != null && !map.isEmpty()) {
            mergeMapInput(map, customSuffixMap);
        }
        return (S) this;
    }

    /**
     * add bean properties as conditions
     *
     * @since 1.2
     *
     */
    public S fromBean(Object bean) {
        return fromBean(bean, null);
    }

    /**
     * add bean properties as conditions
     *
     * @since 1.2
     *
     */
    public S fromBean(Object bean, Map<String, String> customSuffixMap) {
        if (bean != null) {
            mergeMapInput(BeanPropertyUtils.toMap(bean), customSuffixMap);
        }
        return (S) this;
    }

    /**
     * @since 1.1
     * @deprecated use {@link #fromMap(Map)} instead
     */
    @Deprecated
    public S appendByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    /**
     * @since 1.1
     * @deprecated use {@link #fromMap(Map, Map)} instead
     */
    @Deprecated
    public S appendByMap(Map<?, ?> map, Map<String, String> customSuffixMap) {
        return fromMap(map, customSuffixMap);
    }

    /**
     * @since 1.1
     * @deprecated use {@link #fromBean(Object)} instead
     */
    @Deprecated
    public S appendByBean(Object bean) {
        return fromBean(bean);
    }

    /**
     * @since 1.1
     * @deprecated use {@link #fromBean(Object, Map)} instead
     */
    @Deprecated
    public S appendByBean(Object bean, Map<String, String> customSuffixMap) {
        return fromBean(bean, customSuffixMap);
    }

    /**
     * @deprecated use {@link #fromMap(Map)} instead
     */
    @Deprecated
    public S appendEqByMap(Map<?, ?> map) {
        return fromMap(map);
    }

    /**
     * @deprecated use {@link #fromBean(Object)} instead
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
