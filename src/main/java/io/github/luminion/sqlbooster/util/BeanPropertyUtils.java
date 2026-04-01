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

public abstract class BeanPropertyUtils {

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

    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    public static Map<String, PropertyDescriptor> getPropertyMap(Class<?> beanClass) {
        if (beanClass == null) {
            return Collections.emptyMap();
        }
        return PD_CACHE.get(beanClass);
    }

    public static Set<String> getPropertyNames(Class<?> beanClass) {
        return getPropertyMap(beanClass).keySet();
    }

    public static <T> T copyProperties(Object sourceBean, T targetBean) {
        if (sourceBean == null || targetBean == null) {
            return targetBean;
        }
        BeanUtils.copyProperties(sourceBean, targetBean);
        return targetBean;
    }

    public static <T> T toTarget(Object sourceBean, Class<T> targetBeanClass) {
        if (sourceBean == null) {
            return null;
        }
        return copyProperties(sourceBean, newInstance(targetBeanClass));
    }

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
            // 一些查询 bean 会把 getter 设成非 public，这里统一放开访问限制。
            ReflectionUtils.makeAccessible(readMethod);
            Object value = readMethod.invoke(bean);
            if (value != null) {
                map.put(pd.getName(), value);
            }
        }
        return map;
    }

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
