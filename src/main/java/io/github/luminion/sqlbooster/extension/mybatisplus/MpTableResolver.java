package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import io.github.luminion.sqlbooster.core.TableResolver;
import io.github.luminion.sqlbooster.function.GetterReference;
import io.github.luminion.sqlbooster.util.SerializedLambdaUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * 基于 Mybatis-Plus 的 {@link TableResolver} 实现。
 * <p>
 * 利用 Mybatis-Plus 的缓存和注解信息来提供表名、字段映射等元数据。
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class MpTableResolver implements TableResolver {
    private final int priority;

    @Override
    public <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        return SerializedLambdaUtils.resolveGetterPropertyName(getter);
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getKeyProperty();
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        Map<String, String> map = tableInfo.getFieldList().stream()
                .collect(Collectors.toMap(TableFieldInfo::getProperty,
                        e -> String.format("a.%s", e.getColumn())));
        map.put(tableInfo.getKeyProperty(), String.format("a.%s", tableInfo.getKeyColumn()));
        return map;
    }

    @Override
    public <T> String getTableName(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getTableName();
    }

    @Override
    public int getPriority() {
        return priority;
    }
}
