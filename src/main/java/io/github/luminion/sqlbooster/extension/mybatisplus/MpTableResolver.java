package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import io.github.luminion.sqlbooster.metadata.TableMeta;
import io.github.luminion.sqlbooster.metadata.TableResolver;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 基于 MyBatis-Plus 的表元数据解析器。
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class MpTableResolver implements TableResolver {

    private final int priority;

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public <T> TableMeta resolve(Class<T> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        if (tableInfo == null) {
            return null;
        }
        List<TableFieldInfo> fieldList = tableInfo.getFieldList();
        Map<String, String> propertyToColumnAliasMap = null;
        if (fieldList != null && !fieldList.isEmpty()) {
            propertyToColumnAliasMap = fieldList.stream()
                    .collect(Collectors.toMap(TableFieldInfo::getProperty,
                            e -> String.format("a.%s", e.getColumn()),
                            (left, right) -> left,
                            LinkedHashMap::new));
        }
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty != null) {
            if (propertyToColumnAliasMap == null) {
                propertyToColumnAliasMap = new LinkedHashMap<>();
            }
            propertyToColumnAliasMap.put(keyProperty, String.format("a.%s", tableInfo.getKeyColumn()));
        }
        return new TableMeta(tableInfo.getTableName(), keyProperty, propertyToColumnAliasMap);
    }
}