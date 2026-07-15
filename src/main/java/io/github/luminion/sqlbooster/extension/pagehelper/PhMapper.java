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
        if (pageNum < 1 || pageNum > Integer.MAX_VALUE || pageSize < 1 || pageSize > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("pageNum and pageSize must be between 1 and "
                    + Integer.MAX_VALUE);
        }
        // PageHelper 内部以 int 计算 startRow = (pageNum-1)*pageSize，两个参数各自合法但乘积仍可能溢出成负数，
        // 生成 LIMIT 负数 的非法 SQL，这里提前用 long 校验乘积上界。
        if ((pageNum - 1) * pageSize > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("pageNum * pageSize offset overflows int: pageNum="
                    + pageNum + ", pageSize=" + pageSize);
        }
        // PageHelper 的 startPage 仅接收 int，这里保留 long 只是为了和统一的 Booster 分页接口对齐。
        try (Page<V> objects = PageHelper.startPage((int) pageNum, (int) pageSize)) {
            PageInfo<V> pageInfo = objects.doSelectPageInfo(() -> doFetch(sqlContext, null));
            return new PhPage<>(pageInfo);
        }
    }
}
