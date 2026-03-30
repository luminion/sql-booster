package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.LambdaBooster;

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

    public <V> LambdaBooster<T, V> boost(Booster<T, V> booster) {
        return new LambdaBooster<>(booster).append(this.toSqlContext());
    }
}
