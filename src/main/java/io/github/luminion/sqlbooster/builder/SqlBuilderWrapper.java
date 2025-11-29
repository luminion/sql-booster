package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.core.TableMetaRegistry;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;
import java.util.Optional;

/**
 * 一个可执行查询的 SQL 构建器包装类。
 * <p>
 * 提供了 {@code first()}, {@code list()}, {@code page()} 等终端操作来立即执行查询。
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 */
@SuppressWarnings("unused")
public class SqlBuilderWrapper<T, V> extends LambdaSqlBuilder<T, SqlBuilderWrapper<T, V>> {
    private final Booster<T, V> booster;

    public SqlBuilderWrapper(Booster<T, V> booster) {
        super(TableMetaRegistry.getEntityClass(booster));
        this.booster = booster;
    }

    public SqlBuilderWrapper(Booster<T, V> booster, SqlContext<T> sqlContext) {
        super(TableMetaRegistry.getEntityClass(booster));
        this.booster = booster;
        this.sqlContext.merge(sqlContext);
    }

    @Override
    protected SqlBuilderWrapper<T, V> newInstance() {
        return new SqlBuilderWrapper<>(booster);
    }

    public V first() {
        return booster.voFirst(this.sqlContext);
    }

    public <R> R first(Class<R> targetType) {
        return booster.voFirst(this.sqlContext, targetType);
    }

    public Optional<V> firstOpt() {
        return booster.voFirstOpt(this.sqlContext);
    }

    public <R> Optional<R> firstOpt(Class<R> targetType) {
        return booster.voFirstOpt(this.sqlContext, targetType);
    }

    public V unique() {
        return booster.voUnique(this.sqlContext);
    }

    public <R> R unique(Class<R> targetType) {
        return booster.voUnique(this.sqlContext, targetType);
    }

    public Optional<V> uniqueOpt() {
        return booster.voUniqueOpt(this.sqlContext);
    }

    public <R> Optional<R> uniqueOpt(Class<R> targetType) {
        return booster.voUniqueOpt(this.sqlContext, targetType);
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
