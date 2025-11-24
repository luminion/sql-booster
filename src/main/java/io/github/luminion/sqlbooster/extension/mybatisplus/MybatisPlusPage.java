package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.luminion.sqlbooster.core.BoosterPage;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * Mybatis-Plus 分页适配对象
 *
 * @param <T> 记录的类型
 * @author luminion
 * @since 1.0.0
 */
@RequiredArgsConstructor
public class MybatisPlusPage<T> implements BoosterPage<T> {

    /**
     * Mybatis-Plus 的分页对象
     */
    private final IPage<T> pageInfo;

    @Override
    public List<T> getRecords() {
        return pageInfo.getRecords();
    }

    @Override
    public long getTotal() {
        return pageInfo.getTotal();
    }

    @Override
    public long getCurrent() {
        return pageInfo.getCurrent();
    }

    @Override
    public long getSize() {
        return pageInfo.getSize();
    }

    @Override
    public <R> BoosterPage<R> convertRecords(Class<R> targetType) {
        IPage<R> convert = pageInfo.convert(e -> ReflectUtils.toTarget(e, targetType));
        return new MybatisPlusPage<>(convert);
    }

}
