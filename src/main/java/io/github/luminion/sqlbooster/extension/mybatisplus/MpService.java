package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 针对 Mybatis-Plus 的扩展 Service 接口。
 * <p>
 * 集成了 {@link IService} 和 sql-booster 的查询能力。
 */
public interface MpService<T, V> extends IService<T>, MpBooster<T, V> {
}
