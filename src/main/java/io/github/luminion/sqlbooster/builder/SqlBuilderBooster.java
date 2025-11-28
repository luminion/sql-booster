package io.github.luminion.sqlbooster.builder;

import io.github.luminion.sqlbooster.core.BoosterApi;
import io.github.luminion.sqlbooster.model.BoosterPage;
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
public class SqlBuilderBooster<T, V> extends LambdaBuilder<T, SqlBuilderBooster<T, V>> {
    private final BoosterApi<T, V> boosterApi;

    public SqlBuilderBooster(BoosterApi<T, V> boosterApi) {
        super(TableInfoUtils.getEntityClass(boosterApi));
        this.boosterApi = boosterApi;
    }
    
    @Override
    public SqlBuilderBooster<T, V> newInstance() {
        return new SqlBuilderBooster<>(boosterApi);
    }

    public V first() {
        return boosterApi.voFirst(this.sqlContext);
    }

    public V first(Class<V> targetType) {
        return boosterApi.voFirst(this.sqlContext, targetType);
    }

    public Optional<V> firstOpt() {
        return boosterApi.voFirstOpt(this.sqlContext);
    }

    public V unique() {
        return boosterApi.voUnique(this.sqlContext);
    }

    public V unique(Class<V> targetType) {
        return boosterApi.voUnique(this.sqlContext, targetType);
    }

    public Optional<V> uniqueOpt() {
        return boosterApi.voUniqueOpt(this.sqlContext);
    }

    public List<V> list() {
        return boosterApi.voList(this.sqlContext);
    }

    public <R> List<R> list(Class<R> targetType) {
        return boosterApi.voList(this.sqlContext, targetType);
    }

    public BoosterPage<V> page(int pageNum, int pageSize) {
        return boosterApi.voPage(this.sqlContext, pageNum, pageSize);
    }

    public BoosterPage<V> page(long pageNum, long pageSize) {
        return boosterApi.voPage(this.sqlContext, pageNum, pageSize);
    }

    public <R> BoosterPage<R> page(int pageNum, int pageSize, Class<R> targetType) {
        return boosterApi.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

    public <R> BoosterPage<R> page(long pageNum, long pageSize, Class<R> targetType) {
        return boosterApi.voPage(this.sqlContext, pageNum, pageSize, targetType);
    }

}