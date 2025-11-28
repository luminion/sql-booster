package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.SFunc;
import io.github.luminion.sqlbooster.util.TableInfoUtils;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * 基础的 Provider 实现
 * <p>
 * 提供了默认的表名、ID 属性名、getter 属性名和属性到列的映射逻辑.
 *
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class BasicTableResolver implements TableResolver {

    /**
     * 是否将驼峰命名转换为下划线命名
     */
    private final boolean mapUnderscoreToCamelCase;
    private final int priority;

    @Override
    public <T> String getTableName(Class<T> clazz) {
        String tableName = TableInfoUtils.camelCaseToUnderscore(clazz.getSimpleName());
        if (tableName.startsWith("_")){
            return tableName.substring(1);
        }
        return tableName;
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        try {
            clazz.getMethod("getId");
            return "id";
        } catch (NoSuchMethodException e) {
            return null;
        }
    }

    @Override
    public <T, R> String getGetterPropertyName(SFunc<T, R> getter) {
        try {
            return ReflectUtils.getGetterPropertyName(getter);
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        Set<String> strings = ReflectUtils.fieldMap(clazz).keySet();
        return strings.stream()
                .collect(Collectors.toMap(e -> e, e -> 
                        String.format("a.%s", mapUnderscoreToCamelCase ? TableInfoUtils.camelCaseToUnderscore(e) : e)));
    }

    @Override
    public int getPriority() {
        return priority;
    }

}
