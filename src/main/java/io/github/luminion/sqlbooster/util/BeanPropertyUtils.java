package io.github.luminion.sqlbooster.util;

import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.util.ReflectionUtils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * 反射操作工具类。
 * <p>
 * 提供实例创建、属性复制、Bean 属性解析、方法引用解析等通用反射功能。
 */
public abstract class BeanPropertyUtils {

    /**
     * 使用 ClassValue 缓存 Bean 的属性描述符。
     * ClassValue 是 JDK 提供的用于关联 Class 数据的标准方式，能自动处理弱引用和 GC。
     */
    private static final ClassValue<Map<String, PropertyDescriptor>> PD_CACHE = new ClassValue<Map<String, PropertyDescriptor>>() {
        @Override
        protected Map<String, PropertyDescriptor> computeValue(Class<?> clazz) {
            PropertyDescriptor[] pds = BeanUtils.getPropertyDescriptors(clazz);
            Map<String, PropertyDescriptor> map = new LinkedHashMap<>(pds.length);
            for (PropertyDescriptor pd : pds) {
                if ("class".equals(pd.getName())) {
                    continue;
                }
                map.put(pd.getName(), pd);
            }
            return Collections.unmodifiableMap(map);
        }
    };

    /**
     * 创建指定类型的实例。
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
     * 结果已过滤 "class" 属性，且为不可变 Map。
     *
     * @param beanClass Bean 的 Class
     * @return 属性名 -> PropertyDescriptor 的映射
     */
    public static Map<String, PropertyDescriptor> getPropertyMap(Class<?> beanClass) {
        if (beanClass == null) {
            return Collections.emptyMap();
        }
        return PD_CACHE.get(beanClass);
    }

    /**
     * 获取 Bean 的所有属性名集合。
     */
    public static Set<String> getPropertyNames(Class<?> beanClass) {
        return getPropertyMap(beanClass).keySet();
    }

    /**
     * 属性复制 (浅拷贝)。
     *
     * @param sourceBean 源对象
     * @param targetBean 目标对象
     * @return 目标对象
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
     * 规则：
     * 1. 忽略值为 null 的属性。
     * 2. 忽略 "class" 属性。
     * 3. 确保私有类/方法的访问权限。
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
        Map<String, Object> map = new LinkedHashMap<>(propertyDescriptorMap.size());

        for (PropertyDescriptor pd : propertyDescriptorMap.values()) {
            Method readMethod = pd.getReadMethod();
            if (readMethod == null) {
                continue;
            }
            ReflectionUtils.makeAccessible(readMethod);
            Object value = readMethod.invoke(bean);
            if (value != null) {
                map.put(pd.getName(), value);
            }
        }
        return map;
    }

    /**
     * 获取单个属性的值
     *
     * @param bean 对象
     * @param propertyName 属性名
     * @return 属性值
     */
    @SneakyThrows
    public static Object getProperty(Object bean, String propertyName) {
        if (bean == null) return null;
        PropertyDescriptor pd = getPropertyMap(bean.getClass()).get(propertyName);
        if (pd == null || pd.getReadMethod() == null) {
            return null;
        }
        Method readMethod = pd.getReadMethod();
        ReflectionUtils.makeAccessible(readMethod);
        return readMethod.invoke(bean);
    }

}