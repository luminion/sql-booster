package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.util.GenericTypeUtils;

/**
 * SQL查询构建器。提供了用于创建查询的静态工厂方法及多种链式调用。
 *
 * @param <T> 实体类型
 */
@SuppressWarnings("unused")
public class SqlBuilder<T> extends LambdaSqlBuilder<T, SqlBuilder<T>> {

    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        return new SqlBuilder<>(entityClass);
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        return new SqlBuilder<>(GenericTypeUtils.resolveBoosterEntityClass(booster));
    }

    public SqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    @Override
    protected SqlBuilder<T> newInstance() {
        return new SqlBuilder<>(this.entityClass);
    }

    /**
     * 将当前构建的条件与一个 Booster 实例关联，并返回一个可以执行查询的包装器。
     *
     * @param booster Booster 实例
     * @return 用于执行查询的 SqlBuilderWrapper
     */
    public <V, P> SqlBuilderWrapper<T, V> boost(Booster<T, V> booster) {
        return new SqlBuilderWrapper<>(booster, this.sqlContext);
    }
}
