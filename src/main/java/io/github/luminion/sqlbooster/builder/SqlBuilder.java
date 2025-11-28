package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.BoosterParam;
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
public class SqlBuilder<T> extends LambdaBuilder<T, SqlBuilder<T>> {
    
    public static <T> SqlBuilder<T> of(Class<T> entityClass) {
        return new SqlBuilder<>(entityClass);
    }

    public static <T> SqlBuilder<T> of(Booster<T, ?> booster) {
        return new SqlBuilder<>(TableInfoUtils.getEntityClass(booster));
    }

    public static <T> SqlBuilder<T> of(BoosterParam<T> boosterParam) {
        return new SqlBuilder<>(TableInfoUtils.getEntityClass(boosterParam));
    }

    public SqlBuilder(Class<T> entityClass) {
        super(entityClass);
    }

    @Override
    public SqlBuilder<T> newInstance() {
        return new SqlBuilder<>(this.entityClass);
    }
}