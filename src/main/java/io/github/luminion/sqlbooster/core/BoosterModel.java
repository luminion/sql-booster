package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.builder.LambdaBooster;

/**
 * 实体侧可选接口，用于声明默认的 Booster 结果类型。
 *
 * @param <T> 实体自身类型
 * @param <V> 默认结果类型
 */
public interface BoosterModel<T extends BoosterModel<T, V>, V> {

    @SuppressWarnings("unchecked")
    default Booster<T, V> booster() {
        return EntityBoosters.booster((Class<T>) this.getClass());
    }

    @SuppressWarnings("unchecked")
    default <R> Booster<T, R> booster(Class<R> targetType) {
        return EntityBoosters.booster((Class<T>) this.getClass(), targetType);
    }

    @SuppressWarnings("unchecked")
    default LambdaBooster<T, V> lambdaBooster() {
        return EntityBoosters.lambda((Class<T>) this.getClass());
    }

    @SuppressWarnings("unchecked")
    default <R> LambdaBooster<T, R> lambdaBooster(Class<R> targetType) {
        return EntityBoosters.lambda((Class<T>) this.getClass(), targetType);
    }
}