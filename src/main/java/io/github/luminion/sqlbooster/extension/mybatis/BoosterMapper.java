package io.github.luminion.sqlbooster.extension.mybatis;

import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.SqlContext;
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.util.SqlContextUtils;

import java.util.List;

/**
 * 基于原生 MyBatis 的通用 Mapper 接口，用于执行动态 VO 查询。
 * 默认不提供分页实现, 分页查询方法需子类自行实现。
 */
public interface BoosterMapper<T, V> extends BoosterSupport<T, V> {

    @Override
    default List<V> selectByBooster(SqlContext<T> boosterParam, Object page) {
        SqlContext<T> sqlContext = SqlBuilder.of(this)
                .append(boosterParam)
                .build(SqlContextUtils::buildWithSuffix);
        return selectByXml(sqlContext, page);
    }

    /**
     * 该方法需要由对应的 Mapper XML 文件实现。
     *
     * @param sqlContext 经过解析和校验的 SQL 上下文
     * @param page       分页插件对象 (例如 PageHelper 的 Page)
     * @return 查询结果列表
     */
    List<V> selectByXml(SqlContext<T> sqlContext, Object page);
}
