package io.github.luminion.sqlbooster.model.api;

import java.util.Collection;
import java.util.Map;

/**
 * 查询包装器接口.
 * <p>
 * 扩展自 {@link Tree} 接口, 增加了排序和额外查询条件的功能.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
public interface Wrapper<T> extends Tree {

    /**
     * 获取排序规则列表.
     *
     * @return 排序规则列表
     * @since 1.0.0
     */
    Collection<Sort> getSorts();

    /**
     * 获取不属于实体本身的额外查询条件.
     * <p>
     * key 为条件名, value 为条件值.
     *
     * @return 额外查询条件 Map
     * @since 1.0.0
     */
    Map<String,Object> getExtra();

}