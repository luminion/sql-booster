package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.LambdaBooster;

/**
 * 只负责“构造 `SqlContext`”的入口。
 * 适合先把条件拼好，再交给 Mapper / Service / Booster 去执行。
 */
@SuppressWarnings("unused")
public class SqlBuilder<T> extends LambdaSqlBuilder<T, SqlBuilder<T>> {

    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        return new SqlBuilder<>(entityClass);
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        return new SqlBuilder<>(booster.boosterEntityClass());
    }

    public SqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    @Override
    protected SqlBuilder<T> newInstance() {
        return new SqlBuilder<>(this.entityClass);
    }

    /**
     * 把当前已构造的条件切换到某个具体 Booster 上继续执行。
     */
    public <V> LambdaBooster<T, V> boost(Booster<T, V> booster) {
        return new LambdaBooster<>(booster).append(this.toSqlContext());
    }
}
