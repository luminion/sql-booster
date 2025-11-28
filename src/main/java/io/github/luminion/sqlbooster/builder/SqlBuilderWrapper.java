package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.util.TableInfoUtils;

import java.util.List;
import java.util.Optional;

/**
 * 具备扩展查询功能的 SQL 构建助手.
 * <p>
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SqlBuilderWrapper<T, V> extends LambdaSqlBuilder<T, SqlBuilderWrapper<T, V>> {
    private final Booster<T, V> booster;

    public SqlBuilderWrapper(Booster<T, V> booster) {
        super(TableInfoUtils.getEntityClass(booster));
        this.booster = booster;
    }

    public SqlBuilderWrapper(Booster<T, V> booster, SqlContext<T> sqlContext) {
        super(TableInfoUtils.getEntityClass(booster));
        this.booster = booster;
        this.sqlContext.merge(sqlContext);
    }

    @Override
    public SqlBuilderWrapper<T, V> newInstance() {
        return new SqlBuilderWrapper<>(booster);
    }

    public V first() {
        return booster.voFirst(this.sqlContext);
    }

    public V first(Class<V> targetType) {
        return booster.voFirst(this.sqlContext, targetType);
    }

    public Optional<V> firstOpt() {
        return booster.voFirstOpt(this.sqlContext);
    }

    public V unique() {
        return booster.voUnique(this.sqlContext);
    }

    public V unique(Class<V> targetType) {
        return booster.voUnique(this.sqlContext, targetType);
    }

    public Optional<V> uniqueOpt() {
        return booster.voUniqueOpt(this.sqlContext);
    }

    public List<V> list() {
        return booster.voList(this.sqlContext);
    }

    public <R> List<R> list(Class<R> targetType) {
        return booster.voList(this.sqlContext, targetType);
    }

    public BPage<V> page(int pageNum, int pageSize) {
        return booster.voPage(this.sqlContext, pageNum, pageSize);
    }

    public BPage<V> page(long pageNum, long pageSize) {
        return booster.voPage(this.sqlContext, pageNum, pageSize);
    }

    public <R> BPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return booster.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

    public <R> BPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return booster.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

}