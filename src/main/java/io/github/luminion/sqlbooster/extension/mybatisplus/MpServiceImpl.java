package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

/**
 * {@link MpService} 的抽象实现基类。
 * 继承自 MyBatis-Plus 的 {@link ServiceImpl}，并复用 service 层的 booster 查询门面能力。
 *
 * @param <M> Mapper 类型
 * @param <T> 实体类型
 * @param <V> 默认结果类型
 */
public abstract class MpServiceImpl<M extends MpMapper<T, V>, T, V> extends ServiceImpl<M, T>
        implements MpService<T, V> {
}