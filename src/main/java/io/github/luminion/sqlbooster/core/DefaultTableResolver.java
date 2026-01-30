package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.function.GetterReference;
import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import io.github.luminion.sqlbooster.util.SerializedLambdaUtils;
import io.github.luminion.sqlbooster.util.StrConvertUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * 默认的 {@link TableResolver} 实现。
 * <p>
 * 基于通用的命名约定（如类名转表名、"id"作为主键）来提供元信息。
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class DefaultTableResolver implements TableResolver {

    private final boolean underscoreToCamelCase;

    @Override
    public <T> String getTableName(Class<T> clazz) {
        return StrConvertUtils.pascalCaseToUnderscore(clazz.getSimpleName());
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
    public <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        return SerializedLambdaUtils.resolveGetterPropertyName(getter);
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        Set<String> beanPropertyNameSet = BeanPropertyUtils.getPropertyNames(clazz);
        return beanPropertyNameSet.stream().collect(Collectors.toMap(
                e -> e,
                e -> String.format("a.%s", underscoreToCamelCase ? StrConvertUtils.camelCaseToUnderscore(e) : e)
        ));
    }

}
