package io.github.luminion.sqlbooster.util;

import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 反射操作工具类。
 * <p>
 * 提供实例创建、属性复制、Bean 属性解析、方法引用解析等通用反射功能。
 */
public abstract class BeanPropertyUtils {

    private static final Map<Class<?>, Map<String, PropertyDescriptor>> BEAN_PROPERTY_MAP_CACHE = new ConcurrentHashMap<>();

    /**
     * 创建指定类型的实例。
     * 使用 Spring BeanPropertyUtils.instantiateClass，内部已处理无参构造等细节。
     */
    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    /**
     * 获取 Bean 的属性映射：propertyName -> PropertyDescriptor。
     * <p>
     * Spring 的 BeanPropertyUtils.getPropertyDescriptors(beanClass) 已经做了 Class 级缓存，
     * 这里在其基础上仅缓存我们封装的 Map，避免每次都 new HashMap+遍历。
     *
     * @param beanClass Bean 的 Class
     * @return 属性名 -> PropertyDescriptor 的映射，未做防变更包装，按需自行 copy/unmodifiable。
     */
    public static Map<String, PropertyDescriptor> getPropertyMap(Class<?> beanClass) {
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
            // BeanPropertyUtils 已经过滤了 getClass()，这里正常加入即可
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
    public static Set<String> getPropertyNames(Class<?> beanClass) {
        return Collections.unmodifiableSet(getPropertyMap(beanClass).keySet());
    }

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

    /**
     * 将一个 JavaBean 对象转换为 Map。
     * <p>
     * 使用 PropertyDescriptor 的 readMethod 读取属性值，忽略为 null 的属性。
     *
     * @param bean 源对象
     * @return 转换后的 Map
     */
    @SneakyThrows
    public static Map<String, Object> toMap(Object bean) {
        if (bean == null) {
            return Collections.emptyMap();
        }
        Map<String, PropertyDescriptor> propertyDescriptorMap = getPropertyMap(bean.getClass());
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

}