package io.github.luminion.sqlbooster.core;

import java.io.Serializable;
import java.util.function.Function;

/**
 * 可序列化的方法引用.
 *
 * @param <T> 输入类型
 * @param <R> 返回类型
 * @author luminion
 * @since 1.0.0
 */
@FunctionalInterface
public interface Getter<T, R> extends Function<T, R>, Serializable {

}
