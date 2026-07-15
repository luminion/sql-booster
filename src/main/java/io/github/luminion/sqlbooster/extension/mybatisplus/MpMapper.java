package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * 针对 MyBatis-Plus 的扩展 Mapper 接口。
 * 集成了 {@link BaseMapper} 和 sql-booster 的查询能力。
 */
public interface MpMapper<T, V> extends BaseMapper<T>, BoosterMapper<T, V> {

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        if (pageNum < 1 || pageSize < 1) {
            throw new IllegalArgumentException("pageNum and pageSize must be positive, but got pageNum="
                    + pageNum + ", pageSize=" + pageSize);
        }
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> results = doFetch(sqlContext, pageInfo);
        pageInfo.setRecords(results);
        return new MpPage<>(pageInfo);
    }
}