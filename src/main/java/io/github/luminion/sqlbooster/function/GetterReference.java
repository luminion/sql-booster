package io.github.luminion.sqlbooster.function;

import java.io.Serializable;
import java.util.function.Function;

/**
 * 可序列化的方法引用 (Getter)。
 * <p>
 * 用于在 Lambda 表达式中安全地传递属性信息，以便在运行时解析属性名。
 *
 * @param <T> 输入类型
 * @param <R> 返回类型
 */
@FunctionalInterface
public interface GetterReference<T, R> extends Function<T, R>, Serializable {

}
