package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

public interface PhMapper<T, V> extends BoosterMapper<T, V> {

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        // PageHelper 的 startPage 仅接收 int，这里保留 long 只是为了和统一的 Booster 分页接口对齐。
        // 实际接入场景会先在上层约束分页参数范围，因此这里不额外做溢出分支处理。
        try (Page<V> objects = PageHelper.startPage((int) pageNum, (int) pageSize)) {
            PageInfo<V> pageInfo = objects.doSelectPageInfo(() -> doFetch(sqlContext, null));
            return new PhPage<>(pageInfo);
        }
    }
}
