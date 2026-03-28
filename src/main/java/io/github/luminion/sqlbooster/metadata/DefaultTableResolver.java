package io.github.luminion.sqlbooster.metadata;

import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import io.github.luminion.sqlbooster.util.StrConvertUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 默认的表元数据解析器。
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class DefaultTableResolver implements TableResolver {

    private final boolean underscoreToCamelCase;
    private final int priority;

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public <T> TableMeta resolve(Class<T> clazz) {
        String tableName = StrConvertUtils.pascalCaseToUnderscore(clazz.getSimpleName());
        String idPropertyName = null;
        try {
            clazz.getMethod("getId");
            idPropertyName = "id";
        } catch (NoSuchMethodException e) {
            // ignore
        }
        Set<String> beanPropertyNameSet = BeanPropertyUtils.getPropertyNames(clazz);
        Map<String, String> propertyToColumnAliasMap = beanPropertyNameSet.stream().collect(Collectors.toMap(
                e -> e,
                e -> String.format("a.%s", underscoreToCamelCase ? StrConvertUtils.camelCaseToUnderscore(e) : e)
        ));
        return new TableMeta(tableName, idPropertyName, propertyToColumnAliasMap);
    }
}