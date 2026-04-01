package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaSqlBuilder;
import io.github.luminion.sqlbooster.model.BPage;

import java.util.List;

/**
 * “边写条件边执行”的入口。
 * 和 `SqlBuilder` 的区别是：这里已经绑定了具体 Booster，末尾可以直接 `list/first/page`。
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

    /**
     * 直接复用当前 builder 内部持有的 `sqlContext` 执行查询。
     */
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
