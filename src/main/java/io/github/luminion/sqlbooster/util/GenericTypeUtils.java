package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.core.Booster;
import org.springframework.core.GenericTypeResolver;

/**
 * @author luminion
 * @since 1.0.0
 */
public abstract class GenericTypeUtils {

    /**
     * 解析泛型参数类型。
     * 直接委托给 Spring 的 GenericTypeResolver。
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> genericIfc) {
        return GenericTypeResolver.resolveTypeArguments(clazz, genericIfc);
    }

    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<T> resolveBoosterEntityClass(Booster<T, V> booster) {
        return (Class<T>) resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    @SuppressWarnings({"unchecked"})
    public static <T, V> Class<V> resolveBoosterVoClass(Booster<T, V> booster) {
        return (Class<V>) resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }
}
