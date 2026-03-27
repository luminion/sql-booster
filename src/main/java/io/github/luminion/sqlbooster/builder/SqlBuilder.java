package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.SqlContext;

/**
 * SQL 查询构建器。
 *
 * @param <T> 实体类型
 */
@SuppressWarnings("unused")
public class SqlBuilder<T> extends LambdaSqlBuilder<T, SqlBuilder<T>> {

    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        return new SqlBuilder<>(entityClass);
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        return new SqlBuilder<>(booster.entityClass());
    }

    public SqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    @Override
    protected SqlBuilder<T> newInstance() {
        return new SqlBuilder<>(this.entityClass);
    }

    public SqlContext<T> toSqlContext() {
        return super.toSqlContext();
    }

    public <V> LambdaBooster<T, V> boost(Booster<T, V> booster) {
        return new LambdaBooster<>(booster).mergeSegment( this.toSqlContext());
    }
}
