package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

/**
 * 基于 PageHelper 的 Mapper 接口。
 */
public interface PhMapper<T, V> extends BoosterMapper<T, V> {

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        try (Page<V> objects = PageHelper.startPage((int) pageNum, (int) pageSize)) {
            PageInfo<V> pageInfo = objects.doSelectPageInfo(() -> doFetch(sqlContext, null));
            return new PhPage<>(pageInfo);
        }
    }
}