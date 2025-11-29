package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

/**
 * 针对 PageHelper 分页插件的 {@link BoosterSupport} 扩展接口。
 * <p>
 * 提供了基于 PageHelper 的 {@code voPage} 分页查询实现。
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 */
public interface PhBooster<T, V> extends BoosterSupport<T, V> {

    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        PageInfo<V> pageInfo = PageHelper.startPage((int) pageNum, (int) pageSize)
                .doSelectPageInfo(() -> selectByBooster(sqlContext, null));
        return new PhPage<>(pageInfo);
    }
}
