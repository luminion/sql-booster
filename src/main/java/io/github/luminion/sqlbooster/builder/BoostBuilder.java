package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.BoosterApi;
import io.github.luminion.sqlbooster.model.BoosterPage;

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
    private final BoosterApi<T, V> boosterApi;

    @Override
    public BoostBuilder<T, V> newInstance() {
        return new BoostBuilder<>(boosterApi);
    }

    public BoostBuilder(BoosterApi<T, V> boosterApi) {
        this.boosterApi = boosterApi;
    }

    public V first() {
        return boosterApi.voFirst(this);
    }

    public V first(Class<V> targetType) {
        return boosterApi.voFirst(this, targetType);
    }

    public Optional<V> firstOpt() {
        return boosterApi.voFirstOpt(this);
    }

    public V unique() {
        return boosterApi.voUnique(this);
    }

    public V unique(Class<V> targetType) {
        return boosterApi.voUnique(this, targetType);
    }

    public Optional<V> uniqueOpt() {
        return boosterApi.voUniqueOpt(this);
    }

    public List<V> list() {
        return boosterApi.voList(this);
    }

    public <R> List<R> list(Class<R> targetType) {
        return boosterApi.voList(this, targetType);
    }

    public BoosterPage<V> page(int pageNum, int pageSize) {
        return boosterApi.voPage(this, pageNum, pageSize);
    }

    public BoosterPage<V> page(long pageNum, long pageSize) {
        return boosterApi.voPage(this, pageNum, pageSize);
    }

    public <R> BoosterPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return boosterApi.voPage(this, pageNum, pageSize, targetType);
    }

    public <R> BoosterPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return boosterApi.voPage(this, pageNum, pageSize, targetType);
    }

}