package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.GetterReference;

import java.util.Map;

/**
 * 表信息解析器接口。
 * <p>
 * 定义了如何从实体类中获取数据库元信息（如表名、主键、字段映射等）的规范。
 * 如果未找到对应的元信息，将返回 null。
 * 
 */
public interface TableResolver {

    /**
     * 根据实体类获取表名。
     */
    <T> String getTableName(Class<T> entityClass);

    /**
     * 根据实体类获取主键属性名。
     */
    <T> String getIdPropertyName(Class<T> entityClass);

    /**
     * 从 getter 方法引用中获取属性名。
     */
    <T, R> String getGetterPropertyName(GetterReference<T, R> getter);
    
    /**
     * 获取实体类的属性到数据库列名(或别名)的映射。
     */
    <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> entityClass);
}
