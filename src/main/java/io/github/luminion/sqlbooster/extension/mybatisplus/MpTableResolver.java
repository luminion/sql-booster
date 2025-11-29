package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import io.github.luminion.sqlbooster.function.GetterReference;
import io.github.luminion.sqlbooster.core.TableResolver;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * MyBatis-Plus 提供者实现.
 * <p>
 * 基于 MyBatis-Plus 框架提供数据库元数据信息，包括表名、字段映射等.
 *
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
@EqualsAndHashCode
public class MpTableResolver implements TableResolver {
    private final int priority;

    @Override
    public <T, R> String getGetterPropertyName(GetterReference<T, R> getter) {
        return ReflectUtils.getGetterPropertyName(getter);
    }

    @Override
    public <T> String getIdPropertyName(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getKeyProperty();
    }

    @Override
    public <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz) {
        return TableInfoHelper.getTableInfo(clazz).getFieldList().stream()
                .collect(Collectors.toMap(TableFieldInfo::getProperty,
                        e -> String.format("a.%s", e.getColumn())));
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
