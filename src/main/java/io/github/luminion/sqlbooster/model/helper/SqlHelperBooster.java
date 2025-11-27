package io.github.luminion.sqlbooster.model.helper;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.core.BoosterParam;

import java.util.List;
import java.util.Optional;

/**
 * 具备扩展查询功能的 SQL 构建助手.
 * <p>
 * 封装了 {@link BoosterCore} 和 {@link AbstractHelper}, 提供了方便的链式调用查询方法.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public class SqlHelperBooster<T, V> extends LambdaHelper<T, SqlHelperBooster<T, V>> {
    private final BoosterCore<T, V> boosterCore;

    @Override
    public SqlHelperBooster<T, V> newInstance() {
        return new SqlHelperBooster<>(boosterCore);
    }

    public SqlHelperBooster(BoosterCore<T, V> boosterCore) {
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