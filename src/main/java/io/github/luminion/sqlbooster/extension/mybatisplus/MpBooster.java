package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.plugins.pagination.PageDTO;
import io.github.luminion.sqlbooster.core.BoosterSupport;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * 针对 Mybatis-Plus 分页插件的 {@link BoosterSupport} 扩展接口。
 * <p>
 * 提供了基于 Mybatis-Plus 的 {@code voPage} 分页查询实现。
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 */
public interface MpBooster<T, V> extends BoosterSupport<T, V> {
    @Override
    default BPage<V> voPage(SqlContext<T> sqlContext, long pageNum, long pageSize) {
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(sqlContext, pageInfo);
        pageInfo.setRecords(vs);
        return new MpPage<>(pageInfo);
    }
}
