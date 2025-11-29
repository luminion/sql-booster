package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.GetterReference;

import java.util.Map;

/**
 * 表信息解析器接口。
 * <p>
 * 定义了如何从实体类中获取数据库元信息（如表名、主键、字段映射等）的规范。
 * 框架可以同时存在多个实现，通过优先级进行加载。
 */
public interface TableResolver extends Comparable<TableResolver>{

    @Override
    default int compareTo(TableResolver o){
        return o.getPriority() - getPriority();
    }

    /**
     * 获取当前解析器的优先级。数字越小，优先级越高。
     */
    int getPriority();

    /**
     * 根据实体类获取表名。
     *
     * @param clazz 实体类
     * @return 数据库表名
     */
    <T> String getTableName(Class<T> clazz);


    /**
     * 根据实体类获取主键属性名。
     *
     * @param clazz 实体类
     * @return 主键属性名
     */
    <T> String getIdPropertyName(Class<T> clazz);

    /**
     * 从 getter 方法引用中获取属性名。
     *
     * @param getter getter 方法引用
     * @return 属性名
     */
    <T, R> String getGetterPropertyName(GetterReference<T, R> getter);
    
    /**
     * 获取实体类的属性到数据库列名(或别名)的映射。
     *
     * @param clazz 实体类
     * @return 属性名到列名的映射 Map
     */
    <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz);
}
