package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.function.SFunc;
import lombok.SneakyThrows;
import org.springframework.util.ReflectionUtils;

import java.beans.Introspector;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author luminion
 * @since 1.0.0
 */
public abstract class LambdaUtils {
    private static final Map<Class<?>, SerializedLambda> LAMBDA_CACHE = new ConcurrentHashMap<>();

    /**
     * 从可序列化的方法引用中提取 SerializedLambda 信息。
     * <p>
     * 使用 lambdaClass 为 key 做一层缓存，避免每次都反射调用 writeReplace。
     *
     * @param getter 方法引用，例如 User::getName
     * @return SerializedLambda 实例
     */
    @SneakyThrows
    private static <T, R> SerializedLambda resolveSerializedLambda(SFunc<T, R> getter) {
        Class<?> lambdaClass = getter.getClass();
        SerializedLambda cached = LAMBDA_CACHE.get(lambdaClass);
        if (cached != null) {
            return cached;
        }

        Method writeReplaceMethod = lambdaClass.getDeclaredMethod("writeReplace");
        writeReplaceMethod.setAccessible(true);
        SerializedLambda lambda = (SerializedLambda) writeReplaceMethod.invoke(getter);
        LAMBDA_CACHE.put(lambdaClass, lambda);
        return lambda;
    }

    /**
     * 从方法引用中获取其声明所在的类的 Class 对象。
     *
     * @param getter 方法引用
     * @return 声明该方法的类
     */
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> Class<T> resolveGetterClass(SFunc<T, R> getter) {
        SerializedLambda serializedLambda = resolveSerializedLambda(getter);
        String className = serializedLambda.getImplClass().replace("/", ".");
        return (Class<T>) Class.forName(className);
    }

    /**
     * 从方法引用中获取其对应的 Method 对象。
     * <p>
     * 这里不再对 Method 本身做额外缓存，调用频率一般不会高到需要再加一层。
     *
     * @param getter 方法引用
     * @return Method 对象
     */
    @SneakyThrows
    public static <T, R> Method resolveGetterMethod(SFunc<T, R> getter) {
        SerializedLambda serializedLambda = resolveSerializedLambda(getter);
        String implMethodName = serializedLambda.getImplMethodName();
        Class<?> getterClass = resolveGetterClass(getter);
        Method method = ReflectionUtils.findMethod(getterClass, implMethodName);
        if (method == null) {
            throw new IllegalStateException("Could not find method " + implMethodName +
                    " on class " + getterClass.getName());
        }
        return method;
    }

    /**
     * 从 getter 方法引用中获取其对应的属性名。
     *
     * @param getter 方法引用，例如 User::getName
     * @return 属性名，例如 "name"
     */
    public static <T, R> String resolveGetterPropertyName(SFunc<T, R> getter) {
        String implMethodName = resolveSerializedLambda(getter).getImplMethodName();
        String name = implMethodName;

        if (name.startsWith("is")) {
            name = name.substring(2);
        } else if (name.startsWith("get") || name.startsWith("set")) {
            name = name.substring(3);
        } else {
            throw new IllegalArgumentException("Error parsing property name '" + implMethodName +
                    "'. Didn't start with 'is', 'get' or 'set'.");
        }
        return Introspector.decapitalize(name);
    }
}
