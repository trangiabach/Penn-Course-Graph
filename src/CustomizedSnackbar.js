import * as React from 'react';
import Stack from '@mui/material/Stack';
import Button from '@mui/material/Button';
import Snackbar from '@mui/material/Snackbar';
import MuiAlert from '@mui/material/Alert';
import { Typography } from '@mui/material';

const Alert = React.forwardRef(function Alert(
  props,
  ref,
) {
  return <MuiAlert elevation={6} ref={ref} variant="filled" {...props} />;
});

export default function CustomizedSnackbars() {
  const [open, setOpen] = React.useState(false);

  const handleClick = () => {
    setOpen(true);
  };

  const handleClose = (event, reason) => {
    if (reason === 'clickaway') {
      return;
    }

    setOpen(false);
  };

  React.useEffect(() => {
    setOpen(true)
  }, [])

  return (
    <Stack spacing={2} sx={{ width: '100%' }}>
      <Snackbar anchorOrigin={{ vertical: 'top', horizontal: 'center' }} open={open} onClose={handleClose}>
        <Alert onClose={handleClose} severity="primary" sx={{ width: '100%' }}>
          <Typography>
            🖍 Welcome to Penn Course Graph!
          </Typography>
          <Typography>
            🙌 Click/hover each course to see connected courses
          </Typography>
          <Typography>
            💪 Drag your mouse to explore the graph
          </Typography>
          <Typography>
            🍵 Zoom in/out using mousepad or scrolling
          </Typography>
        </Alert>
      </Snackbar>
    </Stack>
  );
}