package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.function.SFunc;
import lombok.SneakyThrows;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.beans.Introspector;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public abstract class LambdaUtils {
    private static final Map<Class<?>, SerializedLambda> LAMBDA_CACHE = new ConcurrentHashMap<>();

    @SneakyThrows
    private static <T, R> SerializedLambda resolveSerializedLambda(SFunc<T, R> getter) {
        Class<?> lambdaClass = getter.getClass();
        SerializedLambda cached = LAMBDA_CACHE.get(lambdaClass);
        if (cached != null) {
            return cached;
        }

        // JDK 会把方法引用编译成 lambda 对象，这里通过 writeReplace 取回底层的 SerializedLambda。
        Method writeReplaceMethod = lambdaClass.getDeclaredMethod("writeReplace");
        writeReplaceMethod.setAccessible(true);
        SerializedLambda lambda = (SerializedLambda) writeReplaceMethod.invoke(getter);
        LAMBDA_CACHE.put(lambdaClass, lambda);
        return lambda;
    }

    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> Class<T> resolveGetterClass(SFunc<T, R> getter) {
        SerializedLambda serializedLambda = resolveSerializedLambda(getter);
        String className = serializedLambda.getImplClass().replace("/", ".");
        // 用 getter 自身的 classloader 解析实体类，兼容 Tomcat 共享 lib、OSGi 等父子加载器隔离场景；
        // Class.forName(name) 会用加载本库的 classloader，可能看不到 webapp 里的实体类。
        return (Class<T>) ClassUtils.forName(className, getter.getClass().getClassLoader());
    }

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

    public static <T, R> String resolveGetterPropertyName(SFunc<T, R> getter) {
        String implMethodName = resolveSerializedLambda(getter).getImplMethodName();
        String name = implMethodName;

        // 这里只接受标准 JavaBean getter，避免把普通方法引用误识别成字段。
        if (name.startsWith("is") && name.length() > 2 && Character.isUpperCase(name.charAt(2))) {
            name = name.substring(2);
        } else if ((name.startsWith("get") || name.startsWith("set"))
                && name.length() > 3 && Character.isUpperCase(name.charAt(3))) {
            name = name.substring(3);
        } else {
            throw new IllegalArgumentException("Error parsing property name '" + implMethodName +
                    "'. Didn't start with 'is', 'get' or 'set'.");
        }
        return Introspector.decapitalize(name);
    }
}
