package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.TableInfoUtils;

/**
 * SQL 构建助手
 * <p>
 * 提供 SQL 查询构建的具体实现, 支持链式调用和 Lambda 表达式.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SqlBuilder<T> extends LambdaSqlBuilder<T, SqlBuilder<T>> {
    
    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        return new SqlBuilder<>(entityClass);
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        return new SqlBuilder<>(TableInfoUtils.getEntityClass(booster));
    }

    public static <T> SqlBuilder<T> of(SqlContext<T> sqlContext) {
        return new SqlBuilder<>(TableInfoUtils.getEntityClass(sqlContext));
    }

    public SqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    @Override
    public SqlBuilder<T> newInstance() {
        return new SqlBuilder<>(this.entityClass);
    }

    /**
     * 转换为 {@link SqlBuilderWrapper}.
     *
     * @param booster {@link Booster} 实例
     * @param <V>        VO 类型
     * @param <P>        分页对象类型
     * @return {@link SqlBuilderWrapper} 实例
     * @since 1.0.0
     */
    public <V, P> SqlBuilderWrapper<T, V> boost(Booster<T, V> booster) {
        return new SqlBuilderWrapper<>(booster, this.sqlContext);
    }
}