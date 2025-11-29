package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * {@link MpService} 的抽象实现基类。
 * <p>
 * 继承自 Mybatis-Plus 的 {@link ServiceImpl}，并将查询委托给对应的 Mapper。
 */
public abstract class MpServiceImpl<M extends MpMapper<T, V>, T, V> extends ServiceImpl<M, T> implements MpService<T, V> {

    @Override
    public List<V> selectByBooster(SqlContext<T> sqlContext, Object page) {
        return getBaseMapper().selectByBooster(sqlContext, page);
    }

}
