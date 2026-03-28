package io.github.luminion.sqlbooster.metadata;

import lombok.Getter;

import java.util.Collections;
import java.util.Map;

/**
 * 实体表元数据。
 */
@Getter
public class TableMeta {

    private final String tableName;
    private final String idPropertyName;
    private final Map<String, String> propertyToColumnAliasMap;

    public TableMeta(String tableName, String idPropertyName, Map<String, String> propertyToColumnAliasMap) {
        this.tableName = tableName;
        this.idPropertyName = idPropertyName;
        this.propertyToColumnAliasMap = propertyToColumnAliasMap == null
                ? Collections.emptyMap()
                : Collections.unmodifiableMap(propertyToColumnAliasMap);
    }

}
