package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.luminion.sqlbooster.model.SqlContext;

import java.util.List;

/**
 * 针对 Mybatis-Plus 的 ServiceImpl的扩展.
 *
 * @author luminion
 * @since 1.0.0
 */
public abstract class MyBatisPlusBoosterServiceImpl<M extends MyBatisPlusBoosterMapper<T, V>, T, V> extends ServiceImpl<M, T> implements MyBatisPlusBoosterService<T, V> {

    @Override
    public List<V> selectByBooster(SqlContext<T> sqlContext, Object page) {
        return getBaseMapper().selectByBooster(sqlContext, page);
    }   

}
