package io.github.luminion.sqlbooster.model.builder;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.core.BoosterPage;

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
public class BoostBuilder<T, V> extends LambdaBuilder<T, BoostBuilder<T, V>> {
    private final BoosterCore<T, V> boosterCore;

    @Override
    public BoostBuilder<T, V> newInstance() {
        return new BoostBuilder<>(boosterCore);
    }

    public BoostBuilder(BoosterCore<T, V> boosterCore) {
        this.boosterCore = boosterCore;
    }

    public V first() {
        return boosterCore.voFirst(this);
    }

    public V first(Class<V> targetType) {
        return boosterCore.voFirst(this, targetType);
    }

    public Optional<V> firstOpt() {
        return boosterCore.voFirstOpt(this);
    }

    public V unique() {
        return boosterCore.voUnique(this);
    }

    public V unique(Class<V> targetType) {
        return boosterCore.voUnique(this, targetType);
    }

    public Optional<V> uniqueOpt() {
        return boosterCore.voUniqueOpt(this);
    }

    public List<V> list() {
        return boosterCore.voList(this);
    }

    public <R> List<R> list(Class<R> targetType) {
        return boosterCore.voList(this, targetType);
    }

    public BoosterPage<V> page(int pageNum, int pageSize) {
        return boosterCore.voPage(this, pageNum, pageSize);
    }

    public BoosterPage<V> page(long pageNum, long pageSize) {
        return boosterCore.voPage(this, pageNum, pageSize);
    }

    public <R> BoosterPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return boosterCore.voPage(this, pageNum, pageSize, targetType);
    }

    public <R> BoosterPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return boosterCore.voPage(this, pageNum, pageSize, targetType);
    }

}