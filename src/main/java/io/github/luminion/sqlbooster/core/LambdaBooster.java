package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaSqlBuilder;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * 基于 Lambda 条件构建的增强查询执行器。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
@SuppressWarnings("unused")
public class LambdaBooster<T, V> extends LambdaSqlBuilder<T, LambdaBooster<T, V>> {
    private final Booster<T, V> booster;

    public LambdaBooster(Booster<T, V> booster) {
        super(booster.boosterEntityClass());
        this.booster = booster;
    }

    @Override
    protected LambdaBooster<T, V> newInstance() {
        return new LambdaBooster<>(booster);
    }

    public V first() {
        return booster.voFirst(this.sqlContext);
    }

    public V unique() {
        return booster.voUnique(this.sqlContext);
    }

    public List<V> list() {
        return booster.voList(this.sqlContext);
    }

    public BPage<V> page(long pageNum, long pageSize) {
        return booster.voPage(this.sqlContext, pageNum, pageSize);
    }
}