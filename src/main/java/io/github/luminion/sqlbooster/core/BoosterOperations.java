package io.github.luminion.sqlbooster.core;

import io.github.luminion.sqlbooster.enums.SqlKeyword;
import io.github.luminion.sqlbooster.metadata.TableMetaRegistry;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.model.Condition;
import org.apache.ibatis.exceptions.TooManyResultsException;
import org.springframework.util.ObjectUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Booster 的通用操作扩展接口。
 * 为派生查询契约提供默认实现。
 *
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public interface BoosterOperations<T, V> extends Booster<T, V> {

    @Override
    default List<V> voByIds(Collection<? extends Serializable> ids) {
        if (ids == null || ids.isEmpty()) {
            return new ArrayList<>();
        }
        String idPropertyName = TableMetaRegistry.getIdPropertyName(boosterEntityClass());
        if (ObjectUtils.isEmpty(idPropertyName)) {
            throw new IllegalStateException("can't find id property");
        }
        SqlContext<T> sqlContext = new SqlContext<>();
        sqlContext.getConditions().add(new Condition(idPropertyName, SqlKeyword.IN.getSymbol(), ids));
        return voList(sqlContext);
    }

    @Override
    default V voFirst(SqlContext<T> sqlContext) {
        List<V> results = voList(sqlContext);
        if (results.isEmpty()) {
            return null;
        }
        return results.get(0);
    }

    @Override
    default V voUnique(SqlContext<T> sqlContext) {
        List<V> results = voList(sqlContext);
        if (results.isEmpty()) {
            return null;
        }
        if (results.size() > 1) {
            throw new TooManyResultsException("error query => expected one but found " + results.size());
        }
        return results.get(0);
    }
}
