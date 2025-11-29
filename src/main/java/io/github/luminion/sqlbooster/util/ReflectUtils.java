package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.function.GetterReference;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;
import org.springframework.util.ReflectionUtils;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * 反射操作工具类。
 * <p>
 * 提供实例创建、属性复制、方法引用解析等通用反射功能。
 */
public abstract class ReflectUtils {

    public static boolean isJavaCoreClass(Class<?> clazz) {
        if (clazz == null) {
            return false;
        }
        return clazz.getClassLoader() == null;
    }

    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    /**
     * 判断一个对象是否为 JavaBean。
     * <p>
     * 排除基本类型、Map、Collection、Array，并检查是否存在符合规范的 getter/setter 方法。
     */
    @SneakyThrows
    public static boolean isJavaBean(Object object) {
        if (object == null) {
            return false;
        }
        Class<?> clazz = object.getClass();

        if (BeanUtils.isSimpleProperty(clazz)) {
            return false;
        }

        if (Map.class.isAssignableFrom(clazz) ||
                Collection.class.isAssignableFrom(clazz) ||
                clazz.isArray()) {
            return false;
        }

        BeanInfo beanInfo = Introspector.getBeanInfo(clazz, Object.class);
        PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
        return propertyDescriptors != null && propertyDescriptors.length > 0;
    }

    public static Map<String, Field> fieldMap(Class<?> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        if (isJavaCoreClass(clazz)) {
            throw new IllegalArgumentException("clazz must not be java class");
        }
        Map<String, Field> map = new HashMap<>();
        ReflectionUtils.doWithFields(clazz,
                field -> map.putIfAbsent(field.getName(), field),
                ReflectionUtils.COPYABLE_FIELDS);
        return map;
    }

    public static <T> T copyProperties(Object source, T target) {
        if (source == null || target == null) return target;
        BeanUtils.copyProperties(source, target);
        return target;
    }

    /**
     * 将一个 JavaBean 对象转换为 Map。
     *
     * @param bean 源对象, 必须为 JavaBean
     * @return 转换后的 Map
     */
    @SneakyThrows
    public static Map<String, Object> javaBeanToMap(Object bean) {
        if (!isJavaBean(bean)) {
            throw new IllegalArgumentException("bean must be java bean");
        }
        Map<String, Object> map = new HashMap<>();
        BeanInfo beanInfo = Introspector.getBeanInfo(bean.getClass(), Object.class);
        PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
        for (PropertyDescriptor property : propertyDescriptors) {
            String key = property.getName();
            Method getter = property.getReadMethod();
            if (getter != null) {
                Object value = getter.invoke(bean);
                if (value != null) {
                    map.put(key, value);
                }
            }
        }
        return map;
    }

    public static <T> T toTarget(Object source, Class<T> clazz) {
        if (source == null) {
            return null;
        }
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return copyProperties(source, newInstance(clazz));
    }

    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> superClass) {
        return GenericTypeResolver.resolveTypeArguments(clazz, superClass);
    }

    /**
     * 从可序列化的方法引用中提取 {@link SerializedLambda} 信息。
     *
     * @param getter 方法引用
     * @return {@link SerializedLambda} 实例
     */
    @SneakyThrows
    private static <T, R> SerializedLambda getSerializedLambda(GetterReference<T, R> getter) {
        Method writeReplaceMethod = getter.getClass().getDeclaredMethod("writeReplace");
        writeReplaceMethod.setAccessible(true);
        return (SerializedLambda) writeReplaceMethod.invoke(getter);
    }

    /**
     * 从方法引用中获取其声明所在的类的 {@link Class} 对象。
     *
     * @param getter 方法引用
     * @return 声明该方法的类的 {@link Class} 对象
     */
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> Class<T> getGetterClass(GetterReference<T, R> getter) {
        SerializedLambda serializedLambda = getSerializedLambda(getter);
        String className = serializedLambda.getImplClass().replace("/", ".");
        return (Class<T>) Class.forName(className);
    }

    /**
     * 从方法引用中获取其对应的 {@link Method} 对象。
     *
     * @param getter 方法引用
     * @return {@link Method} 对象
     */
    @SneakyThrows
    public static <T, R> Method getGetterMethod(GetterReference<T, R> getter) {
        SerializedLambda serializedLambda = getSerializedLambda(getter);
        String implMethodName = serializedLambda.getImplMethodName();
        Class<?> getterClass = getGetterClass(getter);
        Method method = ReflectionUtils.findMethod(getterClass, implMethodName);
        if (method == null) {
            throw new IllegalStateException("Could not find method " + implMethodName);
        }
        return method;
    }

    /**
     * 从 getter 方法引用中获取其对应的属性名。
     *
     * @param getter 方法引用
     * @return 属性名
     */
    @SneakyThrows
    public static <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        String name = getSerializedLambda(getter).getImplMethodName();
        if (name.startsWith("is")) {
            name = name.substring(2);
        } else if (name.startsWith("get") || name.startsWith("set")) {
            name = name.substring(3);
        } else {
            throw new IllegalArgumentException("Error parsing property name '" + name +
                    "'.  Didn't start with 'is', 'get' or 'set'.");
        }
        if (name.length() == 1 || name.length() > 1 && !Character.isUpperCase(name.charAt(1))) {
            name = name.substring(0, 1).toLowerCase(Locale.ENGLISH) + name.substring(1);
        }
        return name;
    }
}
