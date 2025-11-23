import React, { useState, useEffect } from 'react';
import {
  Box,
  Typography,
  Card,
  CardContent,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  CircularProgress,
  Alert,
} from '@mui/material';
import { Storage as DatabaseIcon } from '@mui/icons-material';
import api from '../../services/api';

interface Props {
  uploadId: string;
  programName: string;
}

const ProgramDatabaseUsageView: React.FC<Props> = ({ uploadId, programName }) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [dbUsage, setDbUsage] = useState<any>(null);

  useEffect(() => {
    loadData();
  }, [uploadId, programName]);

  const loadData = async () => {
    setLoading(true);
    setError(null);

    try {
      const data = await api.getDatabaseUsage(uploadId, programName);
      setDbUsage(data);
      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load database usage');
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="300px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ m: 2 }}>
        {error}
      </Alert>
    );
  }

  if (!dbUsage || dbUsage.total_objects === 0) {
    return (
      <Alert severity="info" sx={{ m: 2 }}>
        No database operations found for program <strong>{programName}</strong>
      </Alert>
    );
  }

  return (
    <Box>
      <Card sx={{ mb: 3 }}>
        <CardContent>
          <Box display="flex" alignItems="center" mb={2}>
            <DatabaseIcon sx={{ mr: 2, fontSize: 40, color: 'primary.main' }} />
            <Box>
              <Typography variant="h5">Database Usage</Typography>
              <Typography variant="body2" color="textSecondary">
                Program: <strong>{programName}</strong>
              </Typography>
            </Box>
          </Box>

          <Box display="flex" gap={3} mt={3}>
            <Box>
              <Typography variant="h3" color="primary">
                {dbUsage.total_objects}
              </Typography>
              <Typography variant="caption" color="textSecondary">
                Database Objects
              </Typography>
            </Box>
          </Box>
        </CardContent>
      </Card>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow sx={{ backgroundColor: '#f5f5f5' }}>
              <TableCell><strong>Object Name</strong></TableCell>
              <TableCell><strong>Type</strong></TableCell>
              <TableCell><strong>Access Type</strong></TableCell>
              <TableCell><strong>Access Mode</strong></TableCell>
              <TableCell align="center"><strong>Access Count</strong></TableCell>
              <TableCell><strong>Lines</strong></TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {dbUsage.database_usage.map((usage: any, idx: number) => (
              <TableRow key={idx} hover>
                <TableCell>
                  <Typography variant="body2" fontWeight="bold">
                    {usage.db_object_name}
                  </Typography>
                </TableCell>
                <TableCell>
                  <Chip
                    label={usage.object_type || 'UNKNOWN'}
                    size="small"
                    color={usage.object_type === 'TABLE' ? 'primary' : 'secondary'}
                  />
                </TableCell>
                <TableCell>
                  <Chip
                    label={usage.access_type || 'UNKNOWN'}
                    size="small"
                    color={
                      usage.access_type === 'READ'
                        ? 'info'
                        : usage.access_type === 'WRITE'
                        ? 'warning'
                        : 'default'
                    }
                  />
                </TableCell>
                <TableCell>
                  {usage.access_mode || 'N/A'}
                </TableCell>
                <TableCell align="center">
                  <Chip label={usage.access_count} size="small" />
                </TableCell>
                <TableCell>
                  <Typography variant="caption" color="textSecondary">
                    {usage.line_numbers?.slice(0, 5).join(', ')}
                    {usage.line_numbers?.length > 5 && '...'}
                  </Typography>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    </Box>
  );
};

export default ProgramDatabaseUsageView;
