package io.github.luminion.sqlbooster.model.helper;

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
public class SqlHelper<T> extends LambdaHelper<T, SqlHelper<T>> {

    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }
    
    public static <T> SqlHelper<T> of(Class<T> entityClass) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = entityClass;
        return sqlHelper;
    }

    public static <T> SqlHelper<T> of(Booster<T, ?> booster) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = BoostUtils.getEntityClass(booster);
        return sqlHelper;
    }

    public static <T> SqlHelper<T> of(QueryParam<T> queryParam) {
        SqlHelper<T> sqlHelper = new SqlHelper<>();
        sqlHelper.entityClass = BoostUtils.getEntityClass(queryParam);
        return sqlHelper;
    }

    @Override
    public SqlHelper<T> newInstance() {
        return new SqlHelper<>();
    }
}