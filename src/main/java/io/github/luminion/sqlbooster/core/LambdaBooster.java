package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaSqlBuilder;
import io.github.luminion.sqlbooster.model.BPage;

import java.util.List;
import java.util.Optional;

/**
 * 基于 Lambda 语法的增强查询执行链。
 *
 * @param <T> 数据库实体类型
 * @param <V> 查询结果的目标类型
 */
@SuppressWarnings("unused")
public class LambdaBooster<T, V> extends LambdaSqlBuilder<T, LambdaBooster<T, V>> {
    private final Booster<T, V> booster;

    public LambdaBooster(Booster<T, V> booster) {
        super(booster.entityClass());
        this.booster = booster;
    }

    @Override
    protected LambdaBooster<T, V> newInstance() {
        return new LambdaBooster<>(booster);
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
