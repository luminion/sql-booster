package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * 基于默认 Booster 的目标类型适配器。
 */
public class TargetTypeBooster<T, V> implements Booster<T, V> {

    private final Class<T> entityClass;
    private final Class<V> resultClass;
    private final Booster<T, ?> delegate;

    public TargetTypeBooster(Class<T> entityClass, Class<V> resultClass, Booster<T, ?> delegate) {
        this.entityClass = entityClass;
        this.resultClass = resultClass;
        this.delegate = delegate;
    }

    @Override
    public Class<T> entityClass() {
        return entityClass;
    }

    @Override
    public Class<V> resultClass() {
        return resultClass;
    }

    @Override
    public V voById(Serializable id) {
        return delegate.voById(id, resultClass);
    }

    @Override
    public <R> R voById(Serializable id, Class<R> targetType) {
        return delegate.voById(id, targetType);
    }

    @Override
    public Optional<V> voByIdOpt(Serializable id) {
        return delegate.voByIdOpt(id, resultClass);
    }

    @Override
    public <R> Optional<R> voByIdOpt(Serializable id, Class<R> targetType) {
        return delegate.voByIdOpt(id, targetType);
    }

    @Override
    public List<V> voListByIds(Collection<? extends Serializable> ids) {
        return delegate.voListByIds(ids, resultClass);
    }

    @Override
    public <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> targetType) {
        return delegate.voListByIds(ids, targetType);
    }

    @Override
    public V voFirst(SqlContext<T> sqlContext) {
        return delegate.voFirst(sqlContext, resultClass);
    }

    @Override
    public <R> R voFirst(SqlContext<T> sqlContext, Class<R> targetType) {
        return delegate.voFirst(sqlContext, targetType);
    }

    @Override
    public Optional<V> voFirstOpt(SqlContext<T> sqlContext) {
        return delegate.voFirstOpt(sqlContext, resultClass);
    }

    @Override
    public <R> Optional<R> voFirstOpt(SqlContext<T> sqlContext, Class<R> targetType) {
        return delegate.voFirstOpt(sqlContext, targetType);
    }

    @Override
    public V voUnique(SqlContext<T> sqlContext) {
        return delegate.voUnique(sqlContext, resultClass);
    }

    @Override
    public <R> R voUnique(SqlContext<T> sqlContext, Class<R> targetType) {
        return delegate.voUnique(sqlContext, targetType);
    }

    @Override
    public Optional<V> voUniqueOpt(SqlContext<T> sqlContext) {
        return delegate.voUniqueOpt(sqlContext, resultClass);
    }

    @Override
    public <R> Optional<R> voUniqueOpt(SqlContext<T> sqlContext, Class<R> targetType) {
        return delegate.voUniqueOpt(sqlContext, targetType);
    }

    @Override
    public List<V> voList() {
        return delegate.voList(null, resultClass);
    }

    @Override
    public List<V> voList(SqlContext<T> sqlContext) {
        return delegate.voList(sqlContext, resultClass);
    }

    @Override
    public <R> List<R> voList(SqlContext<T> sqlContext, Class<R> targetType) {
        return delegate.voList(sqlContext, targetType);
    }

    @Override
    public BPage<V> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize) {
        return delegate.voPage(sqlContext, pageNum, pageSize, resultClass);
    }

    @Override
    public BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        return delegate.voPage(sqlContext, pageNum, pageSize, resultClass);
    }

    @Override
    public <R> BPage<R> voPage(SqlContext<T> sqlContext, int pageNum, int pageSize, Class<R> targetType) {
        return delegate.voPage(sqlContext, pageNum, pageSize, targetType);
    }

    @Override
    public <R> BPage<R> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize, Class<R> targetType) {
        return delegate.voPage(sqlContext, pageNum, pageSize, targetType);
    }
}
