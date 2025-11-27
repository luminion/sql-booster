package io.github.luminion.sqlbooster.provider;

import io.github.luminion.sqlbooster.core.Getter;
import org.springframework.core.Ordered;

import java.util.Map;

/**
 * 表信息提供者接口
 *
 * @author luminion
 * @since 1.0.0
 */
public interface TableInfoProvider extends Ordered{

    /**
     * 根据实体类获取表名.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 数据库表名
     * @since 1.0.0
     */
    <T> String getTableName(Class<T> clazz);


    /**
     * 根据实体类获取主键属性名.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 主键属性名
     * @since 1.0.0
     */
    <T> String getIdPropertyName(Class<T> clazz);

    /**
     * 从 getter 方法引用中获取属性名.
     *
     * @param getter getter 方法引用
     * @param <T>    实体类型
     * @param <R>    属性类型
     * @return 属性名
     * @since 1.0.0
     */
    <T, R> String getGetterPropertyName(Getter<T, R> getter);
    
    /**
     * 获取实体类的属性到数据库列名(别名)的映射.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 属性名到列名(别名)的映射 Map
     * @since 1.0.0
     */
    <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz);
}
