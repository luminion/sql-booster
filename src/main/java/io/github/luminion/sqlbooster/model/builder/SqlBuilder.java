package io.github.luminion.sqlbooster.model.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.QueryParam;
import io.github.luminion.sqlbooster.util.BoostUtils;

/**
 * SQL 构建助手实现类.
 * <p>
 * 提供 SQL 查询构建的具体实现, 支持链式调用和 Lambda 表达式.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SqlBuilder<T> extends LambdaBuilder<T, SqlBuilder<T>> {

    public static <T> SqlBuilder<T> of() {
        return new SqlBuilder<>();
    }
    
    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        SqlBuilder<T> sqlBuilder = new SqlBuilder<>();
        sqlBuilder.entityClass = entityClass;
        return sqlBuilder;
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        SqlBuilder<T> sqlBuilder = new SqlBuilder<>();
        sqlBuilder.entityClass = BoostUtils.getEntityClass(booster);
        return sqlBuilder;
    }

    public static <T> SqlBuilder<T> of(QueryParam<T> queryParam) {
        SqlBuilder<T> sqlBuilder = new SqlBuilder<>();
        sqlBuilder.entityClass = BoostUtils.getEntityClass(queryParam);
        return sqlBuilder;
    }

    @Override
    public SqlBuilder<T> newInstance() {
        return new SqlBuilder<>();
    }
}