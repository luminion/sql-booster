package io.github.luminion.sqlbooster.extension.mybatisplus;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.util.BeanPropertyUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * 将 Mybatis-Plus 的 {@link IPage} 适配为 {@link BPage} 的包装类。
 *
 * @param <T> 记录的类型
 */
@RequiredArgsConstructor
public class MpPage<T> implements BPage<T> {
    private static final long serialVersionUID = 1L;

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
    public <R> BPage<R> convertRecords(Class<R> targetType) {
        IPage<R> convert = pageInfo.convert(e -> BeanPropertyUtils.toTarget(e, targetType));
        return new MpPage<>(convert);
    }

}
