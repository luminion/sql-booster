package io.github.luminion.sqlbooster.util;

import io.github.luminion.sqlbooster.function.GetterReference;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;
import org.springframework.util.ReflectionUtils;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 反射操作工具类。
 * <p>
 * 提供实例创建、属性复制、Bean 属性解析、方法引用解析等通用反射功能。
 */
public abstract class ReflectUtils {

    // ----------------------------------------------------------------------
    // 缓存区
    // ----------------------------------------------------------------------

    private static final Map<Class<?>, Map<String, PropertyDescriptor>> BEAN_PROPERTY_MAP_CACHE = new ConcurrentHashMap<>();

    private static final Map<Class<?>, SerializedLambda> LAMBDA_CACHE = new ConcurrentHashMap<>();


    // ----------------------------------------------------------------------
    // 实例创建 / 类型工具
    // ----------------------------------------------------------------------

    /**
     * 创建指定类型的实例。
     * 使用 Spring BeanUtils.instantiateClass，内部已处理无参构造等细节。
     */
    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    /**
     * 解析泛型参数类型。
     * 直接委托给 Spring 的 GenericTypeResolver。
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> genericIfc) {
        return GenericTypeResolver.resolveTypeArguments(clazz, genericIfc);
    }


    // ----------------------------------------------------------------------
    // Bean 属性工具
    // ----------------------------------------------------------------------

    /**
     * 获取 Bean 的属性映射：propertyName -> PropertyDescriptor。
     * <p>
     * Spring 的 BeanUtils.getPropertyDescriptors(beanClass) 已经做了 Class 级缓存，
     * 这里在其基础上仅缓存我们封装的 Map，避免每次都 new HashMap+遍历。
     *
     * @param beanClass Bean 的 Class
     * @return 属性名 -> PropertyDescriptor 的映射，未做防变更包装，按需自行 copy/unmodifiable。
     */
    public static Map<String, PropertyDescriptor> beanPropertyMap(Class<?> beanClass) {
        if (beanClass == null) {
            throw new IllegalArgumentException("beanClass must not be null");
        }

        Map<String, PropertyDescriptor> cached = BEAN_PROPERTY_MAP_CACHE.get(beanClass);
        if (cached != null) {
            return cached;
        }

        PropertyDescriptor[] pds = BeanUtils.getPropertyDescriptors(beanClass);
        if (pds.length == 0) {
            throw new IllegalStateException("Could not find any property of bean class: " + beanClass.getName());
        }

        Map<String, PropertyDescriptor> map = new HashMap<>(pds.length);
        for (PropertyDescriptor pd : pds) {
            // BeanUtils 已经过滤了 getClass()，这里正常加入即可
            map.put(pd.getName(), pd);
        }
        map = Collections.unmodifiableMap(map);
        BEAN_PROPERTY_MAP_CACHE.put(beanClass, map);
        return map;
    }

    /**
     * 获取 Bean 的所有属性名集合。
     *
     * @param beanClass Bean 的 Class
     * @return 属性名集合（为简化性能，直接返回内部 Map 的 keySet 视图，外部请避免修改）
     */
    public static Set<String> beanPropertyNameSet(Class<?> beanClass) {
        return Collections.unmodifiableSet(beanPropertyMap(beanClass).keySet());
    }

    /**
     * 将一个 JavaBean 对象转换为 Map。
     * <p>
     * 使用 PropertyDescriptor 的 readMethod 读取属性值，忽略为 null 的属性。
     *
     * @param bean 源对象
     * @return 转换后的 Map
     */
    @SneakyThrows
    public static Map<String, Object> beanToMap(Object bean) {
        if (bean == null) {
            return Collections.emptyMap();
        }
        Map<String, PropertyDescriptor> propertyDescriptorMap = beanPropertyMap(bean.getClass());
        Map<String, Object> map = new HashMap<>(propertyDescriptorMap.size());

        for (Map.Entry<String, PropertyDescriptor> entry : propertyDescriptorMap.entrySet()) {
            PropertyDescriptor pd = entry.getValue();
            Method readMethod = pd.getReadMethod();
            if (readMethod == null) {
                continue;
            }
            Object value = readMethod.invoke(bean);
            if (value != null) {
                map.put(entry.getKey(), value);
            }
        }
        return map;
    }


    // ----------------------------------------------------------------------
    // Bean 拷贝
    // ----------------------------------------------------------------------

    /**
     * 属性复制。
     */
    public static <T> T copyProperties(Object sourceBean, T targetBean) {
        if (sourceBean == null || targetBean == null) {
            return targetBean;
        }
        BeanUtils.copyProperties(sourceBean, targetBean);
        return targetBean;
    }

    /**
     * 创建目标类型实例，并将 sourceBean 的属性拷贝到新实例上。
     */
    public static <T> T toTarget(Object sourceBean, Class<T> targetBeanClass) {
        if (sourceBean == null) {
            return null;
        }
        return copyProperties(sourceBean, newInstance(targetBeanClass));
    }


    // ----------------------------------------------------------------------
    // 方法引用（GetterReference）相关工具
    // ----------------------------------------------------------------------

    /**
     * 从可序列化的方法引用中提取 SerializedLambda 信息。
     * <p>
     * 使用 lambdaClass 为 key 做一层缓存，避免每次都反射调用 writeReplace。
     *
     * @param getter 方法引用，例如 User::getName
     * @return SerializedLambda 实例
     */
    @SneakyThrows
    private static <T, R> SerializedLambda resolveSerializedLambda(GetterReference<T, R> getter) {
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
    public static <T, R> Class<T> resolveGetterClass(GetterReference<T, R> getter) {
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
    public static <T, R> Method resolveGetterMethod(GetterReference<T, R> getter) {
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
    public static <T, R> String resolveGetterPropertyName(GetterReference<T, R> getter) {
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