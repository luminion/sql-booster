package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.model.query.Condition;
import io.github.luminion.sqlbooster.model.query.ConditionSegment;
import io.github.luminion.sqlbooster.model.query.Sort;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import io.github.luminion.sqlbooster.util.SqlContextUtils;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.function.BiFunction;

/**
 * 链式助手.
 * <p>
 * 提供了 SQL 构建的基本功能, 包括条件添加、排序设置等, 并通过泛型支持链式调用.
 *
 * @param <T> 实体类型
 * @param <S> 返回类型 (用于支持链式调用)
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings({"unused", "unchecked"})
@RequiredArgsConstructor
public abstract class AbstractSqlBuilder<T, S extends AbstractSqlBuilder<T, S>> {

    /**
     * 关联的实体类, 用于 SQL 校验和处理.
     */
    protected final Class<T> entityClass;
    /**
     * 存放条件的上下文
     */
    protected final SqlContext<T> sqlContext = new SqlContext<>();

    /**
     * 创建一个新的自身实例.
     *
     * @return 新实例
     * @since 1.0.0
     */
    protected abstract S newInstance();

    public SqlContext<T> build() {
        return build(SqlContextUtils::build);
    }

    public SqlContext<T> build(BiFunction<Class<T>, SqlContext<T>, SqlContext<T>> builder) {
        return builder.apply(this.entityClass, this.sqlContext);
    }

    /**
     * 添加一个查询条件.
     */
    public S append(Condition condition) {
        if (condition != null) {
            this.sqlContext.getConditions().add(condition);
        }
        return (S) this;
    }

    /**
     * 添加一个排序规则.
     */
    public S append(Sort sort) {
        if (sort != null) {
            this.sqlContext.getSorts().add(sort);
        }
        return (S) this;
    }

    /**
     * 合并另一个条件节点.
     */
    public S append(ConditionSegment conditionSegment) {
        if (conditionSegment != null) {
            this.sqlContext.merge(conditionSegment);
        }
        return (S) this;
    }

    /**
     * 将map中的key作为condition的field, value作为condition的value, 生成等于的condition并添加到sqlContext中
     *
     */
    public S appendEqByMap(Map<?, ?> map) {
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Object key = entry.getKey();
            Object value = entry.getValue();
            if (value != null) {
                Condition condition = new Condition(key.toString(), SqlKeyword.EQ.getSymbol(), value);
                this.sqlContext.getConditions().add(condition);
            }
        }
        return (S) this;
    }


    /**
     * 根据java bean 对象生成条件, 入参必须为java bean
     *
     * @param entity 包含查询值的实体对象或 Map
     */
    public S appendEqByJavaBean(Object bean) {
        Map<String, Object> stringObjectMap = ReflectUtils.javaBeanToMap(bean);
        this.appendEqByMap(stringObjectMap);
        return (S) this;
    }

}